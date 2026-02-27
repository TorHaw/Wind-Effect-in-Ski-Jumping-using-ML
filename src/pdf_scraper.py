"""
FIS PDF Results Scraper
======================

Authors:
  MIDN Toren Hawk, MIDN Travis Hockin

Purpose:
  Parse FIS Ski Jumping results PDFs and produce a row-per-jump CSV dataset
  for downstream analysis (wind compensation validation, modeling, figures).

Repository assumptions (relative to project root):
  - Input PDFs:
      data/raw/female_PDFs/*.pdf
      data/raw/male_PDFs/*.pdf
  - Output CSVs:
      data/processed/female_WC_21-25.csv
      data/processed/male_WC_21-25.csv

Notes on dataset semantics:
  - Each PDF corresponds to one competition.
  - Each athlete can produce up to 2 jump rows (rounds).
  - Disqualified or incomplete entries may exist; fields may be empty/NA.
  - Script is designed to be robust to PDF formatting differences by trying
    multiple pdfplumber extraction strategies.

Usage examples:
  # Build both datasets with defaults
  python -m src.FIS_PDF_Scraper --both

  # Female only
  python -m src.FIS_PDF_Scraper --sex female

  # Male only, custom output
  python -m src.FIS_PDF_Scraper --sex male --out data/male.csv

Dependencies:
  - pandas
  - pdfplumber
"""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Optional, Tuple, List, Dict

import pandas as pd
import pdfplumber


# =============================================================================
# 0) Constants and Regex
# =============================================================================

# Months that can appear in DOB lines (avoid being misinterpreted as an NSA code)
MONTHS_CAP = {"JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"}

# Common club/team abbreviations that can look like 3-letter NSAs
NSA_FALLBACK_BLACKLIST = {
    "SKI", "CLUB", "TEAM", "SC", "SS", "IF", "IL", "SV", "BK", "HK", "AK",
    "VFL", "WSV", "PSV", "HSK", "CS", "KS", "LKS", "GTK", "SSK", "BTC", "FIS",
}

# A compact set of very common Ski Jumping NSA codes (extend safely over time)
COMMON_NSA_CODES = {
    "AUT", "GER", "JPN", "NOR", "POL", "SLO", "SUI", "FIN", "CZE", "SWE",
    "USA", "CAN", "ITA", "FRA", "SVK", "UKR", "BUL", "KAZ", "KOR", "CHN", "EST", "LAT", "ROU",
}

# Wind Factors: tolerant to spacing/newlines and dash variants; accepts comma decimals
RX_WIND_FACTORS = re.compile(
    r"(?is)\bWind\s*Factors?\b.*?head\s*/\s*tail\b.*?"
    r"([+-]?\d+(?:[.,]\d+)?)\s*/\s*([+-]?\d+(?:[.,]\d+)?)(?:[^0-9]|$).*?"
    r"(?:points?\s*/\s*)?m\s*/?\s*s"
)

# Fallback form (rare): "... headwind factor 1.23 ... tailwind factor 0.98 ..."
RX_WIND_FACTORS_ALT = re.compile(
    r"(?is)\bheadwind\s*factor\b[^0-9+-]*([+-]?\d+(?:[.,]\d+)?)"
    r".*?\btailwind\s*factor\b[^0-9+-]*([+-]?\d+(?:[.,]\d+)?)"
)


# =============================================================================
# 1) Small utility functions
# =============================================================================

def _nums(s: str) -> List[str]:
    """Return numeric tokens (ints/floats with optional sign) from a string."""
    return re.findall(r"[-+]?\d+(?:\.\d+)?", s or "")


def _split_cell_lines(cell: Optional[str]) -> List[str]:
    """Split a PDF table cell into cleaned non-empty lines."""
    if not cell:
        return []
    parts = re.split(r"[\r\n]+", cell)
    return [p.strip() for p in parts if p and p.strip()]


def _read_round_scalar(lines: List[str], i: int) -> Optional[str]:
    """
    Read the i'th scalar value from a list-of-lines cell.
    Some PDFs pack multiple values into a single line; we handle both styles.
    """
    if not lines:
        return None
    if len(lines) > i:
        nn = _nums(lines[i])
        return nn[0] if nn else None
    if len(lines) == 1:
        nn = _nums(lines[0])
        return nn[i] if len(nn) > i else None
    return None


# =============================================================================
# 2) Event-level parsing (location/date/hill factors)
# =============================================================================

def extract_event_info(text: str) -> Dict[str, object]:
    """
    Extract event-level metadata from full PDF text.

    Returns keys that may include:
      Location, Date, HillSize_m, KPoint_m, MeterValue_pts_per_m,
      GateFactor_pts_per_m, HeadWindFactor, TailWindFactor
    """
    info: Dict[str, object] = {}

    # Location (heuristic): "Place (NOR)" etc.
    mloc = re.search(r"([A-Za-zÀ-ÖØ-öø-ÿ\-\.' ]+)\s\([A-Z]{3}\)", text)
    if mloc:
        info["Location"] = mloc.group(1).strip()

    # Date (heuristic): "12 January 2024"
    mdate = re.search(r"(\d{1,2}\s+[A-Za-z]+\s+\d{4})", text)
    if mdate:
        info["Date"] = mdate.group(1)

    # Hill size (common pattern)
    mhs = re.search(r"HILL\s*SIZE\s*HS\s*/\s*\(95%\)\s*(\d+)", text, flags=re.I)
    if mhs:
        info["HillSize_m"] = int(mhs.group(1))
    else:
        # Fallback: "HS 142" or "Hill Size ... 142"
        mhs2 = re.search(r"(?:Hill\s*Size|HS)\s*[:]*\s*HS?\s*(\d+)", text, flags=re.I)
        if mhs2:
            info["HillSize_m"] = int(mhs2.group(1))

    # K-point
    mkp = re.search(r"K-?Point\s*[:]*\s*(\d+)\s*m?", text, flags=re.I)
    if mkp:
        info["KPoint_m"] = int(mkp.group(1))

    # Meter value (points/m)
    mmv = re.search(r"Meter\s*Value\s*[:]*\s*([\d\.,]+)\s*points\s*/\s*m", text, flags=re.I)
    if mmv:
        info["MeterValue_pts_per_m"] = float(mmv.group(1).replace(",", "."))

    # Gate factor (points/m) [some PDFs label this similarly]
    mgf = re.search(r"Gate\s*factor\s*[:]*\s*([\d\.,]+)\s*points\s*/\s*m", text, flags=re.I)
    if mgf:
        info["GateFactor_pts_per_m"] = float(mgf.group(1).replace(",", "."))

    # Wind factors (points/(m/s))
    m = RX_WIND_FACTORS.search(text) or RX_WIND_FACTORS_ALT.search(text)
    if m:
        info["HeadWindFactor"] = float(m.group(1).replace(",", "."))
        info["TailWindFactor"] = float(m.group(2).replace(",", "."))

    return info


# =============================================================================
# 3) Identity parsing (Name, NSA)
# =============================================================================

def parse_identity_block(block: str) -> Tuple[Optional[str], Optional[str]]:
    """
    Parse the identity cell to extract:
      - Name (no bib, no trailing NSA)
      - NSA (3-letter country code)

    Robust to:
      - "bib\\nName\\nNSA\\nclub..." style
      - DOB lines like "16 MAR 1999"
      - club lines with lots of uppercase tokens
    """
    if not block or not block.strip():
        return None, None

    raw = re.sub(r"[ \t]+", " ", block.replace("\r", "\n")).strip()
    lines = [l.strip() for l in re.split(r"\n+", raw) if l.strip()]

    if len(lines) == 1:
        parts = re.split(r"\s{2,}|\s/\s", lines[0])
        if len(parts) > 1:
            lines = [p.strip() for p in parts if p.strip()]

    def is_dob(s: str) -> bool:
        if re.search(r"(?i)\b(DOB|born|birth)\b", s):
            return True
        if re.search(r"\b\d{1,2}[./-]\d{1,2}[./-]\d{2,4}\b", s):
            return True
        if re.search(r"\b\d{1,2}\s+[A-Z]{3}\s+\d{4}\b", s):
            return True
        if re.fullmatch(r"\d{4}", s):
            return True
        return False

    lines = [l for l in lines if not is_dob(l)]

    # All 3-letter ALLCAPS tokens
    all_caps3 = re.findall(r"\b[A-Z]{3}\b", " ".join(lines))
    all_caps3 = [c for c in all_caps3 if c not in MONTHS_CAP]

    candidates = [c for c in all_caps3 if c in COMMON_NSA_CODES]
    if not candidates:
        candidates = [c for c in all_caps3 if c not in NSA_FALLBACK_BLACKLIST]

    nsa = candidates[-1] if candidates else None

    def clean_name_line(s: str) -> str:
        s = re.sub(r"^\d{1,3}[\s.)-]+", "", s).strip()
        if nsa:
            s = re.sub(r"\s+\b" + re.escape(nsa) + r"\b$", "", s).strip()
        m = re.search(r"\s+\b([A-Z]{3})\b$", s)
        if m:
            t = m.group(1)
            if (t in COMMON_NSA_CODES) or (t not in NSA_FALLBACK_BLACKLIST and t not in MONTHS_CAP):
                s = s[:m.start()].strip()
        return s

    def is_club_like(s: str) -> bool:
        up = s.upper()
        if re.search(r"\b(CLUB|TEAM|SKI|SKICLUB|SV|IF|BK|AK|IL|SC|SS|SSK|VFL|WSV|PSV|HSK)\b", up):
            return True
        words = s.split()
        if len(words) >= 3 and sum(w.isupper() for w in words) >= 3:
            return True
        return False

    name = None
    for l in lines:
        cl = clean_name_line(l)
        if not cl or is_club_like(cl):
            continue
        if re.search(r"[a-z]", cl) or len(cl.split()) <= 4:
            name = cl
            break

    if name is None and lines:
        name = clean_name_line(lines[0])

    # final NSA fallback
    if not nsa and lines:
        for l in (lines[0], lines[-1]):
            m = re.search(r"\b([A-Z]{3})\b$", l)
            if m and (m.group(1) not in MONTHS_CAP) and (m.group(1) not in NSA_FALLBACK_BLACKLIST):
                nsa = m.group(1)
                break

    return (name or None), nsa


# =============================================================================
# 4) Round pairing parser (gate/wind columns are tricky)
# =============================================================================

def read_round_pair_from_combined(text: str, round_idx: int) -> Tuple[Optional[float], Optional[float]]:
    """
    Robust pairing for both:
      - Gate / Comp. Pts   (grammar: INT [FLOAT]) repeated
      - Wind / Comp. Pts   (grammar: FLOAT FLOAT) repeated

    Key correctness property:
      If comp points are blank (e.g., Gate == BaseGate), we must NOT shift and
      accidentally consume the next round's value as comp points.
    """
    if not text:
        return (None, None)

    s = " ".join(str(text).split())
    toks = re.findall(r"[-+]?\d+(?:\.\d+)?", s)
    if not toks:
        return (None, None)

    def is_int_token(t: str) -> bool:
        return t.isdigit()

    mode = "gate" if is_int_token(toks[0]) else "wind"

    # WIND mode: [float, float] per round
    if mode == "wind":
        i = round_idx * 2
        a = float(toks[i]) if i < len(toks) else None
        b = float(toks[i + 1]) if i + 1 < len(toks) else None
        return (a, b)

    # GATE mode: INT [FLOAT] groups; missing FLOAT must remain None
    lines = [ln for ln in re.split(r"\r?\n+", str(text)) if ln.strip()]
    if len(lines) >= 2:
        ln = lines[round_idx] if round_idx < len(lines) else ""
        ltoks = re.findall(r"[-+]?\d+(?:\.\d+)?", ln)
        if not ltoks:
            return (None, None)
        gate = float(ltoks[0]) if ltoks[0].isdigit() else None
        gatepts = None
        if len(ltoks) >= 2:
            nxt = ltoks[1]
            looks_float = ("." in nxt) or nxt.startswith("+") or nxt.startswith("-")
            if looks_float:
                try:
                    gatepts = float(nxt)
                except Exception:
                    gatepts = None
        return (gate, gatepts)

    # Single packed line fallback
    pairs: List[Tuple[Optional[float], Optional[float]]] = []
    i = 0
    n = len(toks)
    while i < n:
        g = toks[i]
        if not is_int_token(g):
            break
        gate = float(g)
        gatepts = None
        if i + 1 < n:
            nxt = toks[i + 1]
            looks_float = ("." in nxt) or nxt.startswith("+") or nxt.startswith("-")
            if looks_float:
                try:
                    gatepts = float(nxt)
                except Exception:
                    gatepts = None
                i += 2
            else:
                i += 1
        else:
            i += 1
        pairs.append((gate, gatepts))
        if len(pairs) == 2:
            break

    return pairs[round_idx] if round_idx < len(pairs) else (None, None)


# =============================================================================
# 5) Row parsing: PDF results table -> jump dict(s)
# =============================================================================

def parse_result_row(row: List[Optional[str]]) -> List[Dict[str, object]]:
    """
    Parse one PDF table row into 1–2 jump rows (Round 1, Round 2).
    Returns empty list if the row doesn't look like an athlete row.
    """
    row = [(c.strip() if c else None) for c in row]
    if not row or not any(row):
        return []
    if not re.match(r"^\d+\.?$", (row[0] or "").strip()):
        return []

    rank = (row[0] or "").replace(".", "").strip()
    identity = row[1] or ""
    name, nsa = parse_identity_block(identity)

    speed_lines = _split_cell_lines(row[2] if len(row) > 2 else None)
    dist_lines  = _split_cell_lines(row[3] if len(row) > 3 else None)
    dpts_lines  = _split_cell_lines(row[4] if len(row) > 4 else None)
    jtot_lines  = _split_cell_lines(row[6] if len(row) > 6 else None)

    gate_text = (row[7] if len(row) > 7 else None) or ""
    wind_text = (row[8] if len(row) > 8 else None) or ""

    rtot_lines  = _split_cell_lines(row[9] if len(row) > 9 else None)
    rrank_lines = _split_cell_lines(row[10] if len(row) > 10 else None)

    overall_val = None
    if len(row) > 11:
        ov = _nums(row[11] or "")
        overall_val = ov[0] if ov else (row[11] or None)

    # Determine number of rounds (cap at 2)
    max_rounds = max(
        len(speed_lines) or 0,
        len(dist_lines) or 0,
        len(dpts_lines) or 0,
        len(jtot_lines) or 0,
        len(rtot_lines) or 0,
        len(rrank_lines) or 0,
        1,
    )
    max_rounds = min(max_rounds, 2)

    jumps: List[Dict[str, object]] = []
    for i in range(max_rounds):
        gate, gatepts = read_round_pair_from_combined(gate_text, i)
        windms, windpts = read_round_pair_from_combined(wind_text, i)

        jump: Dict[str, object] = {
            "Date": None,
            "StartTime": None,
            "EndTime": None,
            "Rank": rank,
            "Name": name,
            "NSA": nsa,
            "Round": i + 1,
            "Speed_kmh": _read_round_scalar(speed_lines, i),
            "Distance_m": _read_round_scalar(dist_lines, i),
            "PointsDist": _read_round_scalar(dpts_lines, i),
            "JudgeTotal": _read_round_scalar(jtot_lines, i),
            "Gate": gate,
            "GatePts": gatepts,
            "WindMS": windms,
            "WindPts": windpts,
            "RoundTotal": _read_round_scalar(rtot_lines, i),
            "RoundRank": _read_round_scalar(rrank_lines, i),
            "OverallPoints": overall_val,

            # Weather / event metadata (filled later)
            "BaseWind": None,
            "BaseGate": None,
            "BaseLength": None,
            "WeatherDesc": None,
            "AirTempStart": None,
            "AirTempFinish": None,
            "TrackTempStart": None,
            "TrackTempFinish": None,
            "HumidityStart": None,
            "HumidityFinish": None,
            "TanWindMin": None,
            "TanWindMax": None,
            "TanWindAvg": None,
            "Location": None,
            "HillSize_m": None,
            "KPoint_m": None,
            "MeterValue_pts_per_m": None,
            "GateFactor_pts_per_m": None,
            "HeadWindFactor": None,
            "TailWindFactor": None,
            "SourceFile": None,
        }

        # Only keep jump rows that have *some* content
        if any(v is not None for v in (
            jump["Speed_kmh"], jump["Distance_m"], jump["PointsDist"], jump["JudgeTotal"],
            jump["Gate"], jump["GatePts"], jump["WindMS"], jump["WindPts"],
            jump["RoundTotal"], jump["RoundRank"],
        )):
            jumps.append(jump)

    return jumps


# =============================================================================
# 6) Weather parsing (last page)
# =============================================================================

def parse_weather_string(line: str) -> Dict[str, Optional[str]]:
    """
    Parse one "Competition / Weather Information" line into structured fields.
    If parsing fails, returns all keys set to None.
    """
    out = {
        "StartTime": None, "EndTime": None,
        "BaseWind": None, "BaseGate": None, "BaseLength": None,
        "WeatherDesc": None,
        "AirTempStart": None, "AirTempFinish": None,
        "TrackTempStart": None, "TrackTempFinish": None,
        "HumidityStart": None, "HumidityFinish": None,
        "TanWindMin": None, "TanWindMax": None, "TanWindAvg": None,
    }
    if not line:
        return out

    rx = re.compile(
        r'(?P<label>(?:\d+(?:st|nd|rd|th)\s+Round|Final\s+Round|Qualification))\s+'
        r'(?P<start>\d{2}:\d{2})\s*-\s*(?P<end>\d{2}:\d{2})\s+'
        r'(?P<basewind>[-+]?\d+(?:\.\d+)?)\s+'
        r'(?P<basegate>\d+)\s+'
        r'(?P<baselength>[-+]?\d+(?:\.\d+)?)m\s+'
        r'(?P<weather>.+?)\s+'
        r'(?P<air_start>[-+]?\d+(?:\.\d+)?)\s*/\s*(?P<air_end>[-+]?\d+(?:\.\d+)?)\s+'
        r'(?P<track_start>[-+]?\d+(?:\.\d+)?)\s*/\s*(?P<track_end>[-+]?\d+(?:\.\d+)?)\s+'
        r'(?P<hum_start>\d+)\s*/\s*(?P<hum_end>\d+)\s+'
        r'(?P<tmin>[-+]?\d+(?:\.\d+)?)\s+'
        r'(?P<tmax>[-+]?\d+(?:\.\d+)?)\s+'
        r'(?P<tavg>[-+]?\d+(?:\.\d+)?)$',
        re.I
    )

    m = rx.search(line.strip())
    if not m:
        return out

    out["StartTime"]       = m.group("start")
    out["EndTime"]         = m.group("end")
    out["BaseWind"]        = m.group("basewind")
    out["BaseGate"]        = m.group("basegate")
    out["BaseLength"]      = m.group("baselength")
    out["WeatherDesc"]     = m.group("weather").strip()
    out["AirTempStart"]    = m.group("air_start")
    out["AirTempFinish"]   = m.group("air_end")
    out["TrackTempStart"]  = m.group("track_start")
    out["TrackTempFinish"] = m.group("track_end")
    out["HumidityStart"]   = m.group("hum_start")
    out["HumidityFinish"]  = m.group("hum_end")
    out["TanWindMin"]      = m.group("tmin")
    out["TanWindMax"]      = m.group("tmax")
    out["TanWindAvg"]      = m.group("tavg")
    return out


def parse_weather_page(pdf: pdfplumber.PDF) -> List[str]:
    """
    Extract candidate weather lines from the last page.
    We stop at "Statistics" / "Legend" to avoid parsing the lower stats table.
    """
    if not pdf.pages:
        return []

    last_txt = pdf.pages[-1].extract_text() or ""
    wsec = re.search(
        r"Competition\s*/\s*Weather Information(.*?)(?:Statistics|Legend|$)",
        last_txt,
        flags=re.S | re.I
    )
    if not wsec:
        return []

    weather_lines: List[str] = []
    for line in wsec.group(1).splitlines():
        lc = " ".join(line.split())
        if not lc:
            continue
        if re.search(r"^(?:\d+(?:st|nd|rd|th)|Final)\s+Round\b", lc, flags=re.I) or re.search(r"^Qualification\b", lc, flags=re.I):
            weather_lines.append(lc)

    return weather_lines


# =============================================================================
# 7) Table extraction (pdfplumber strategies)
# =============================================================================

def choose_results_table(tables: Optional[List[List[List[Optional[str]]]]]) -> Optional[List[List[Optional[str]]]]:
    """
    Select the best candidate table that contains a Rank/Name header.
    """
    for t in tables or []:
        if not t:
            continue
        header = " ".join([(c or "") for c in t[0]])
        if re.search(r"\bRANK\b", header) and re.search(r"\bNAME\b", header):
            return t
    return None


def parse_results_page_generic(page: pdfplumber.page.Page) -> List[Dict[str, object]]:
    """
    Parse one PDF page: try multiple extraction settings to robustly obtain tables.
    """
    rows: List[Dict[str, object]] = []

    settings_list = [
        dict(vertical_strategy="lines", horizontal_strategy="lines", snap_tolerance=3, join_tolerance=3, intersection_tolerance=3),
        dict(vertical_strategy="text",  horizontal_strategy="lines", snap_tolerance=3, join_tolerance=3, intersection_tolerance=3),
        dict(vertical_strategy="lines", horizontal_strategy="text",  snap_tolerance=3, join_tolerance=3, intersection_tolerance=3),
        dict(vertical_strategy="text",  horizontal_strategy="text",  snap_tolerance=3, join_tolerance=3, intersection_tolerance=3),
    ]

    for ts in settings_list:
        try:
            tables = page.extract_tables(table_settings=ts)
        except Exception:
            tables = []

        t = choose_results_table(tables)
        if not t:
            continue

        # Typically: row 0 header, row 1 subheader, row 2+ data
        for body in t[2:]:
            rows.extend(parse_result_row(body))

        if rows:
            break

    return rows


# =============================================================================
# 8) Parse one PDF -> DataFrame
# =============================================================================

def parse_single_pdf(pdf_path: Path) -> pd.DataFrame:
    """
    Parse a single competition PDF into a row-per-jump DataFrame.
    """
    rows: List[Dict[str, object]] = []

    with pdfplumber.open(pdf_path) as pdf:
        full_text = "\n".join(page.extract_text() or "" for page in pdf.pages)
        event_info = extract_event_info(full_text)

        # Parse results pages (excluding last page which is often weather/statistics)
        for page in pdf.pages[:-1]:
            rows.extend(parse_results_page_generic(page))

        weather_lines = parse_weather_page(pdf)

    df = pd.DataFrame(rows)

    if df.empty:
        return df

    # Fill event metadata columns
    defaults: Dict[str, object] = {
        "Location": None, "Date": None,
        "HillSize_m": None, "KPoint_m": None,
        "MeterValue_pts_per_m": None, "GateFactor_pts_per_m": None,
        "HeadWindFactor": None, "TailWindFactor": None,
    }
    defaults.update(event_info)
    df = df.assign(**defaults)

    # Weather: apply one line to all rows, or one per round if possible
    if len(weather_lines) == 1:
        wx = parse_weather_string(weather_lines[0])
        for k, v in wx.items():
            df[k] = v
    elif len(weather_lines) >= 2:
        wx1 = parse_weather_string(weather_lines[0])
        wx2 = parse_weather_string(weather_lines[1])
        for k in wx1.keys():
            df.loc[df["Round"] == 1, k] = wx1[k]
            df.loc[df["Round"] == 2, k] = wx2[k]

    # Always record source file
    df["SourceFile"] = pdf_path.name
    return df


# =============================================================================
# 9) Folder runner
# =============================================================================

def parse_folder_to_csv(folder_path: Path, output_csv: Path) -> Path:
    """
    Parse all PDFs within folder_path (recursively) and write a single CSV.
    Returns the output CSV path.
    """
    folder_path = folder_path.resolve()
    output_csv = output_csv.resolve()
    output_csv.parent.mkdir(parents=True, exist_ok=True)

    pdf_files = sorted(folder_path.rglob("*.pdf"))
    print(f"Found {len(pdf_files)} PDFs in {folder_path}")

    if not pdf_files:
        print("No PDFs found. (This is not an error if you haven't downloaded them yet.)")
        pd.DataFrame().to_csv(output_csv, index=False)
        return output_csv

    all_dfs: List[pd.DataFrame] = []
    failed = 0

    for pdf_path in pdf_files:
        print(f"Parsing: {pdf_path.name}")
        try:
            df = parse_single_pdf(pdf_path)
            if df is None or df.empty:
                print(f"  -> SKIPPED (no rows extracted)")
                failed += 1
            else:
                print(f"  -> OK ({len(df)} rows)")
            all_dfs.append(df if df is not None else pd.DataFrame())
        except Exception as e:
            print(f"  -> FAILED: {e}")
            all_dfs.append(pd.DataFrame({"SourceFile": [pdf_path.name]}))
            failed += 1

    out = pd.concat(all_dfs, ignore_index=True) if all_dfs else pd.DataFrame()
    out.to_csv(output_csv, index=False)
    print(f"Wrote {len(out)} rows to {output_csv} ({failed}/{len(pdf_files)} PDFs failed/skipped).")
    return output_csv


# =============================================================================
# 10) CLI / Entrypoint
# =============================================================================

@dataclass(frozen=True)
class DatasetConfig:
    sex: str
    pdf_dir: Path
    out_csv: Path


def build_default_config(sex: str) -> DatasetConfig:
    """
    Create default config using repo-relative paths.
    This assumes the script is executed from the project root (recommended).
    """
    sex = sex.lower().strip()
    if sex not in {"female", "male"}:
        raise ValueError("sex must be 'female' or 'male'")

    pdf_dir = Path("data") / "raw" / f"{sex}_PDFs"
    out_csv = Path("data") / "processed" / f"{sex}_WC_21-25.csv"
    return DatasetConfig(sex=sex, pdf_dir=pdf_dir, out_csv=out_csv)


def main(argv: Optional[List[str]] = None) -> int:
    parser = argparse.ArgumentParser(description="Scrape FIS Ski Jumping results PDFs into CSV.")
    g = parser.add_mutually_exclusive_group(required=True)
    g.add_argument("--sex", choices=["female", "male"], help="Which dataset to build.")
    g.add_argument("--both", action="store_true", help="Build both female and male datasets.")

    parser.add_argument("--pdf-dir", type=str, default=None,
                        help="Override input PDF directory (default: data/raw/<sex>_PDFs).")
    parser.add_argument("--out", type=str, default=None,
                        help="Override output CSV path (default: data/<sex>_WC_21-25.csv).")

    args = parser.parse_args(argv)

    # Determine which configs to run
    configs: List[DatasetConfig] = []
    if args.both:
        for sex in ("female", "male"):
            cfg = build_default_config(sex)
            configs.append(cfg)
    else:
        cfg = build_default_config(args.sex)
        configs.append(cfg)

    # Apply overrides (if provided) when running single-sex mode
    if not args.both:
        cfg0 = configs[0]
        pdf_dir = Path(args.pdf_dir) if args.pdf_dir else cfg0.pdf_dir
        out_csv = Path(args.out) if args.out else cfg0.out_csv
        configs = [DatasetConfig(sex=cfg0.sex, pdf_dir=pdf_dir, out_csv=out_csv)]

    # Run
    for cfg in configs:
        print(f"[{cfg.sex}] input:  {cfg.pdf_dir}")
        print(f"[{cfg.sex}] output: {cfg.out_csv}")
        if not cfg.pdf_dir.exists():
            print(f"[{cfg.sex}] WARNING: {cfg.pdf_dir} does not exist yet. Creating it now.")
            cfg.pdf_dir.mkdir(parents=True, exist_ok=True)

        parse_folder_to_csv(cfg.pdf_dir, cfg.out_csv)
        print(f"[{cfg.sex}] done.\n")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())