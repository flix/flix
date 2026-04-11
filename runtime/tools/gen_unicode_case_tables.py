#!/usr/bin/env python3
"""
Generates Zig tables for portable (locale-insensitive) Unicode operations used by the LLVM backend runtime.

We target the same Unicode version as the JDK used by our CI/dev toolchain.
As of 2026, JDK 21 uses Unicode 15.0.0.

Inputs (downloaded from unicode.org):
  - UnicodeData.txt              (simple upper/lower/title mappings; general categories; numeric properties)
  - SpecialCasing.txt            (full upper/lower mappings; unconditional only)
  - DerivedCoreProperties.txt    (Cased / Case_Ignorable / Lowercase / Uppercase)
  - PropList.txt                 (White_Space / Ideographic)
  - Blocks.txt                   (Unicode block fallback names)

Output:
  runtime/src/unicode_case_tables.zig

Notes:
  - We intentionally skip conditional special casing rules except `Final_Sigma`,
    which is implemented algorithmically using derived properties.
  - The output is deterministic and can be used across LLVM-native and LLVM-wasm.
"""

from __future__ import annotations

import argparse
import dataclasses
import pathlib
import re
import sys
import textwrap
import urllib.request


UNICODE_VERSION = "15.0.0"
UCD_BASE_URL = f"https://unicode.org/Public/{UNICODE_VERSION}/ucd/"


def fetch_text(url: str) -> str:
    with urllib.request.urlopen(url) as resp:
        data = resp.read()
    return data.decode("utf-8")


def strip_comment(line: str) -> str:
    return line.split("#", 1)[0].strip()


@dataclasses.dataclass(frozen=True)
class Range:
    start: int
    end: int


def merge_ranges(ranges: list[Range]) -> list[Range]:
    if not ranges:
        return []
    ranges = sorted(ranges, key=lambda r: (r.start, r.end))
    out: list[Range] = []
    cur_s, cur_e = ranges[0].start, ranges[0].end
    for r in ranges[1:]:
        if r.start <= cur_e + 1:
            cur_e = max(cur_e, r.end)
        else:
            out.append(Range(cur_s, cur_e))
            cur_s, cur_e = r.start, r.end
    out.append(Range(cur_s, cur_e))
    return out


@dataclasses.dataclass(frozen=True)
class UnicodeTables:
    # Simple (1:1) case mappings from UnicodeData.txt.
    upper_simple: dict[int, int]
    lower_simple: dict[int, int]
    title_simple: dict[int, int]

    # Category-derived sets (BMP only).
    letter_ranges: list[Range]
    digit_ranges: list[Range]
    titlecase_ranges: list[Range]
    alphabetic_ranges: list[Range]
    defined_ranges: list[Range]
    ideographic_ranges: list[Range]
    mirrored_ranges: list[Range]

    # Digit values (BMP only). Uses UnicodeData.txt decimal digit value if present, else digit value.
    digit_value: dict[int, int]  # cp -> 0..9

    # Numeric values (BMP only). Integers map to their value; non-integers map to -2.
    numeric_int_value: dict[int, int]  # cp -> int
    numeric_nonint: set[int]  # cp with numeric value but not a nonnegative integer (fractions etc)
    explicit_names: dict[int, str]  # cp -> UnicodeData name (excluding pseudo names like <control>)


@dataclasses.dataclass(frozen=True)
class Block:
    start: int
    end: int
    name: str


_UNICODEDATA_FIRST_RE = re.compile(r",\s*First>\s*$")
_UNICODEDATA_LAST_RE = re.compile(r",\s*Last>\s*$")


def parse_unicode_data_tables(text: str) -> UnicodeTables:
    upper: dict[int, int] = {}
    lower: dict[int, int] = {}
    title: dict[int, int] = {}

    letter_ranges: list[Range] = []
    digit_ranges: list[Range] = []
    titlecase_ranges: list[Range] = []
    alphabetic_ranges: list[Range] = []
    defined_ranges: list[Range] = []
    ideographic_ranges: list[Range] = []
    mirrored_ranges: list[Range] = []

    digit_value: dict[int, int] = {}
    numeric_int_value: dict[int, int] = {}
    numeric_nonint: set[int] = set()
    explicit_names: dict[int, str] = {}

    pending_range_start: int | None = None
    pending_range_cat: str | None = None
    pending_range_mirrored: str | None = None

    def add_range_sets(start: int, end: int, cat: str, mirrored: str) -> None:
        # Defined (assigned) code points.
        defined_ranges.append(Range(start, end))

        # Category-derived properties (match JDK Character.* semantics for BMP char args).
        if cat in {"Lu", "Ll", "Lt", "Lm", "Lo"}:
            letter_ranges.append(Range(start, end))
        if cat == "Nd":
            digit_ranges.append(Range(start, end))
        if cat == "Lt":
            titlecase_ranges.append(Range(start, end))

        if mirrored == "Y":
            mirrored_ranges.append(Range(start, end))

    for raw in text.splitlines():
        if not raw:
            continue
        fields = raw.split(";")
        if len(fields) < 15:
            continue

        cp = int(fields[0], 16)
        name = fields[1]
        cat = fields[2].strip()

        # Numeric related (UnicodeData fields 6,7,8) and mirrored (field 9).
        dec_str = fields[6].strip()
        dig_str = fields[7].strip()
        num_str = fields[8].strip()
        mirrored = fields[9].strip()

        # Case mapping fields (12,13,14).
        upper_hex = fields[12].strip()
        lower_hex = fields[13].strip()
        title_hex = fields[14].strip()

        if upper_hex:
            upper[cp] = int(upper_hex, 16)
        if lower_hex:
            lower[cp] = int(lower_hex, 16)
        if title_hex:
            title[cp] = int(title_hex, 16)

        if name and not (name.startswith("<") and name.endswith(">")):
            explicit_names[cp] = name

        # UnicodeData has a few explicit <..., First>/<..., Last> range pairs.
        if _UNICODEDATA_FIRST_RE.search(name):
            pending_range_start = cp
            pending_range_cat = cat
            pending_range_mirrored = mirrored
            continue
        if _UNICODEDATA_LAST_RE.search(name) and pending_range_start is not None:
            start = pending_range_start
            end = cp
            add_range_sets(start, end, pending_range_cat or cat, pending_range_mirrored or mirrored)
            pending_range_start = None
            pending_range_cat = None
            pending_range_mirrored = None
            continue

        # Otherwise: singleton.
        add_range_sets(cp, cp, cat, mirrored)

        # Digit values (used by Character.digit); prefer decimal digit value, else digit value.
        if dec_str:
            try:
                digit_value[cp] = int(dec_str)
            except ValueError:
                pass
        elif dig_str:
            try:
                digit_value[cp] = int(dig_str)
            except ValueError:
                pass

        # Numeric values (used by Character.getNumericValue).
        if num_str:
            # Java returns -2 for numeric values that are not a nonnegative integer (e.g. "1/2").
            if "/" in num_str or "." in num_str:
                numeric_nonint.add(cp)
            else:
                try:
                    value = int(num_str, 10)
                    if value < 0 or value > 0x7FFFFFFF:
                        numeric_nonint.add(cp)
                    else:
                        numeric_int_value[cp] = value
                except ValueError:
                    numeric_nonint.add(cp)

    # If the file ends with a dangling First>, treat it as a bug in upstream data (should not happen).
    if pending_range_start is not None:
        raise RuntimeError(f"UnicodeData.txt ended with dangling range start at U+{pending_range_start:04X}")

    return UnicodeTables(
        upper_simple=upper,
        lower_simple=lower,
        title_simple=title,
        letter_ranges=merge_ranges(letter_ranges),
        digit_ranges=merge_ranges(digit_ranges),
        titlecase_ranges=merge_ranges(titlecase_ranges),
        alphabetic_ranges=merge_ranges(alphabetic_ranges),
        defined_ranges=merge_ranges(defined_ranges),
        ideographic_ranges=merge_ranges(ideographic_ranges),
        mirrored_ranges=merge_ranges(mirrored_ranges),
        digit_value=digit_value,
        numeric_int_value=numeric_int_value,
        numeric_nonint=numeric_nonint,
        explicit_names=explicit_names,
    )


def parse_blocks(text: str) -> list[Block]:
    blocks: list[Block] = []
    for raw in text.splitlines():
        line = strip_comment(raw)
        if not line or ";" not in line:
            continue
        left, right = [p.strip() for p in line.split(";", 1)]
        if ".." in left:
            a, b = left.split("..", 1)
            start = int(a, 16)
            end = int(b, 16)
        else:
            start = int(left, 16)
            end = start
        display = right.upper().replace("-", " ")
        blocks.append(Block(start=start, end=end, name=display))
    return blocks


def parse_property_list(text: str, prop: str) -> list[Range]:
    ranges: list[Range] = []
    for raw in text.splitlines():
        line = strip_comment(raw)
        if not line:
            continue
        if ";" not in line:
            continue
        left, right = [p.strip() for p in line.split(";", 1)]
        if right != prop:
            continue
        if ".." in left:
            a, b = left.split("..", 1)
            ranges.append(Range(int(a, 16), int(b, 16)))
        else:
            x = int(left, 16)
            ranges.append(Range(x, x))
    return merge_ranges(ranges)


def subtract_codepoints(ranges: list[Range], points: set[int]) -> list[Range]:
    if not ranges or not points:
        return ranges
    out: list[Range] = []
    points_sorted = sorted(points)
    for r in ranges:
        start = r.start
        end = r.end
        # advance through points in this range.
        for p in points_sorted:
            if p < start:
                continue
            if p > end:
                break
            if p == start:
                start += 1
            else:
                out.append(Range(start, p - 1))
                start = p + 1
        if start <= end:
            out.append(Range(start, end))
    return merge_ranges(out)


_SPECIAL_CASING_LINE = re.compile(r"^[0-9A-Fa-f]{4,6};")


def parse_special_casing(
    text: str,
    simple_upper: dict[int, int],
    simple_lower: dict[int, int],
) -> tuple[dict[int, list[int]], dict[int, list[int]]]:
    """
    Returns:
      - lower_full: unconditional full lowercase mappings (cp -> seq of cps)
      - upper_full: unconditional full uppercase mappings (cp -> seq of cps)

    We skip conditional rules (locale-specific or context-specific) here.
    `Final_Sigma` is handled algorithmically elsewhere.
    """
    lower_full: dict[int, list[int]] = {}
    upper_full: dict[int, list[int]] = {}

    for raw in text.splitlines():
        if not raw or raw.startswith("#"):
            continue
        if not _SPECIAL_CASING_LINE.match(raw):
            continue
        line = strip_comment(raw)
        if not line:
            continue
        parts = [p.strip() for p in line.split(";")]
        if len(parts) < 4:
            continue
        cp = int(parts[0], 16)
        lower_seq = [int(x, 16) for x in parts[1].split()] if parts[1] else []
        # title_seq = parts[2] (unused)
        upper_seq = [int(x, 16) for x in parts[3].split()] if parts[3] else []
        condition = parts[4] if len(parts) >= 5 else ""
        if condition:
            continue

        # Keep only entries that actually differ from simple mappings to reduce table size.
        if lower_seq:
            simple = simple_lower.get(cp, cp)
            if len(lower_seq) != 1 or lower_seq[0] != simple:
                lower_full[cp] = lower_seq
        if upper_seq:
            simple = simple_upper.get(cp, cp)
            if len(upper_seq) != 1 or upper_seq[0] != simple:
                upper_full[cp] = upper_seq

    return lower_full, upper_full


def cp_to_utf16_units(cp: int) -> list[int]:
    if cp <= 0xFFFF:
        return [cp]
    cp -= 0x10000
    hi = 0xD800 + ((cp >> 10) & 0x3FF)
    lo = 0xDC00 + (cp & 0x3FF)
    return [hi, lo]


def zig_u32(x: int) -> str:
    return f"0x{x:08x}"


def zig_u16(x: int) -> str:
    return f"0x{x:04x}"


def format_zig_array(name: str, elem_type: str, values: list[str], columns: int = 8) -> str:
    lines = [f"pub const {name} = [_]{elem_type}{{"]
    for i in range(0, len(values), columns):
        chunk = ", ".join(values[i : i + columns])
        lines.append(f"    {chunk},")
    lines.append("};")
    return "\n".join(lines)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", type=pathlib.Path, default=pathlib.Path("runtime/src/unicode_case_tables.zig"))
    args = ap.parse_args()

    unicode_data = fetch_text(UCD_BASE_URL + "UnicodeData.txt")
    special_casing = fetch_text(UCD_BASE_URL + "SpecialCasing.txt")
    derived_core = fetch_text(UCD_BASE_URL + "DerivedCoreProperties.txt")
    prop_list = fetch_text(UCD_BASE_URL + "PropList.txt")
    blocks_txt = fetch_text(UCD_BASE_URL + "Blocks.txt")

    tables = parse_unicode_data_tables(unicode_data)
    blocks = parse_blocks(blocks_txt)
    cased_ranges = parse_property_list(derived_core, "Cased")
    case_ignorable_ranges = parse_property_list(derived_core, "Case_Ignorable")
    lowercase_ranges = parse_property_list(derived_core, "Lowercase")
    uppercase_ranges = parse_property_list(derived_core, "Uppercase")
    alphabetic_ranges = parse_property_list(derived_core, "Alphabetic")

    # Java's Character.isWhitespace differs from Unicode's White_Space property:
    # - Excludes: U+0085, U+00A0, U+2007, U+202F
    # - Includes: U+001C..U+001F (FS, GS, RS, US)
    whitespace_ranges = parse_property_list(prop_list, "White_Space")
    whitespace_ranges = subtract_codepoints(whitespace_ranges, {0x0085, 0x00A0, 0x2007, 0x202F})
    whitespace_ranges = merge_ranges(whitespace_ranges + [Range(0x001C, 0x001F)])
    ideographic_ranges = parse_property_list(prop_list, "Ideographic")

    lower_full, upper_full = parse_special_casing(special_casing, tables.upper_simple, tables.lower_simple)

    # Emit simple maps as parallel sorted arrays.
    upper_items = sorted(tables.upper_simple.items())
    lower_items = sorted(tables.lower_simple.items())
    title_items = sorted(tables.title_simple.items())
    upper_from = [zig_u32(k) for (k, _) in upper_items]
    upper_to = [zig_u32(v) for (_, v) in upper_items]
    lower_from = [zig_u32(k) for (k, _) in lower_items]
    lower_to = [zig_u32(v) for (_, v) in lower_items]
    title_from = [zig_u32(k) for (k, _) in title_items]
    title_to = [zig_u32(v) for (_, v) in title_items]

    # Emit ranges.
    cased_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in cased_ranges]
    ign_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in case_ignorable_ranges]
    lower_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in lowercase_ranges]
    upper_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in uppercase_ranges]
    alphabetic_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in alphabetic_ranges]

    letter_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in tables.letter_ranges]
    digit_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in tables.digit_ranges]
    titlecase_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in tables.titlecase_ranges]
    defined_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in tables.defined_ranges]
    ideographic_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in ideographic_ranges]
    mirrored_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in tables.mirrored_ranges]
    whitespace_flat = [f".{{ .start = {zig_u32(r.start)}, .end = {zig_u32(r.end)} }}" for r in whitespace_ranges]

    # Emit digit/numeric maps.
    digit_items = sorted(tables.digit_value.items())
    digit_value_from = [zig_u32(k) for (k, _) in digit_items]
    digit_value_to = [str(v) for (_, v) in digit_items]

    numeric_int_items = sorted(tables.numeric_int_value.items())
    numeric_int_from = [zig_u32(k) for (k, _) in numeric_int_items]
    numeric_int_to = [str(v) for (_, v) in numeric_int_items]

    numeric_nonint_keys = sorted(tables.numeric_nonint)
    numeric_nonint_from = [zig_u32(k) for k in numeric_nonint_keys]

    # Explicit UnicodeData names.
    name_items = sorted(tables.explicit_names.items())
    name_from = [zig_u32(k) for (k, _) in name_items]
    name_offsets: list[str] = []
    name_lengths: list[str] = []
    name_bytes: list[str] = []
    offset = 0
    for _, name in name_items:
        bs = name.encode("ascii")
        name_offsets.append(zig_u32(offset))
        name_lengths.append(str(len(bs)))
        name_bytes.extend(str(b) for b in bs)
        offset += len(bs)

    # Unicode block fallback names.
    block_entries: list[str] = []
    block_name_bytes: list[str] = []
    block_name_offset = 0
    for b in blocks:
        bs = b.name.encode("ascii")
        block_entries.append(
            f".{{ .start = {zig_u32(b.start)}, .end = {zig_u32(b.end)}, .name_offset = {zig_u32(block_name_offset)}, .name_len = {len(bs)} }}"
        )
        block_name_bytes.extend(str(x) for x in bs)
        block_name_offset += len(bs)

    # Emit special casing as (key -> slice into flat UTF-16 array).
    def build_special_table(m: dict[int, list[int]]):
        keys = sorted(m.keys())
        values_u16: list[int] = []
        offsets: list[int] = []
        lens: list[int] = []
        for k in keys:
            offsets.append(len(values_u16))
            seq_units: list[int] = []
            for cp in m[k]:
                seq_units.extend(cp_to_utf16_units(cp))
            lens.append(len(seq_units))
            values_u16.extend(seq_units)
        return keys, offsets, lens, values_u16

    lower_keys, lower_offs, lower_lens, lower_vals = build_special_table(lower_full)
    upper_keys, upper_offs, upper_lens, upper_vals = build_special_table(upper_full)

    out_lines: list[str] = []
    out_lines.append("// This file is @generated by runtime/tools/gen_unicode_case_tables.py")
    out_lines.append(
        f"// Source: Unicode {UNICODE_VERSION} (UnicodeData.txt, SpecialCasing.txt, DerivedCoreProperties.txt, PropList.txt, Blocks.txt)"
    )
    out_lines.append("")
    out_lines.append('pub const unicode_version: []const u8 = "' + UNICODE_VERSION + '";')
    out_lines.append("")
    out_lines.append("pub const Range = struct { start: u32, end: u32 };")
    out_lines.append("pub const SpecialCase = struct { key: u32, offset: u32, len: u32 };")
    out_lines.append("pub const UnicodeBlock = struct { start: u32, end: u32, name_offset: u32, name_len: u16 };")
    out_lines.append("")

    out_lines.append(format_zig_array("upper_simple_from", "u32", upper_from))
    out_lines.append("")
    out_lines.append(format_zig_array("upper_simple_to", "u32", upper_to))
    out_lines.append("")
    out_lines.append(format_zig_array("lower_simple_from", "u32", lower_from))
    out_lines.append("")
    out_lines.append(format_zig_array("lower_simple_to", "u32", lower_to))
    out_lines.append("")
    out_lines.append(format_zig_array("title_simple_from", "u32", title_from))
    out_lines.append("")
    out_lines.append(format_zig_array("title_simple_to", "u32", title_to))
    out_lines.append("")

    out_lines.append("pub const cased_ranges = [_]Range{")
    for s in cased_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const case_ignorable_ranges = [_]Range{")
    for s in ign_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const lowercase_ranges = [_]Range{")
    for s in lower_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const uppercase_ranges = [_]Range{")
    for s in upper_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const alphabetic_ranges = [_]Range{")
    for s in alphabetic_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const letter_ranges = [_]Range{")
    for s in letter_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const digit_ranges = [_]Range{")
    for s in digit_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const titlecase_ranges = [_]Range{")
    for s in titlecase_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const whitespace_ranges = [_]Range{")
    for s in whitespace_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const defined_ranges = [_]Range{")
    for s in defined_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const ideographic_ranges = [_]Range{")
    for s in ideographic_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append("pub const mirrored_ranges = [_]Range{")
    for s in mirrored_flat:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")

    out_lines.append(format_zig_array("digit_value_from", "u32", digit_value_from))
    out_lines.append("")
    out_lines.append(format_zig_array("digit_value_to", "u8", digit_value_to))
    out_lines.append("")

    out_lines.append(format_zig_array("numeric_int_from", "u32", numeric_int_from))
    out_lines.append("")
    out_lines.append(format_zig_array("numeric_int_to", "i32", numeric_int_to))
    out_lines.append("")

    out_lines.append(format_zig_array("numeric_nonint_from", "u32", numeric_nonint_from))
    out_lines.append("")

    out_lines.append(format_zig_array("name_from", "u32", name_from))
    out_lines.append("")
    out_lines.append(format_zig_array("name_offsets", "u32", name_offsets))
    out_lines.append("")
    out_lines.append(format_zig_array("name_lengths", "u16", name_lengths))
    out_lines.append("")
    out_lines.append(format_zig_array("name_bytes", "u8", name_bytes, columns=24))
    out_lines.append("")

    out_lines.append("pub const unicode_blocks = [_]UnicodeBlock{")
    for s in block_entries:
        out_lines.append(f"    {s},")
    out_lines.append("};")
    out_lines.append("")
    out_lines.append(format_zig_array("unicode_block_name_bytes", "u8", block_name_bytes, columns=24))
    out_lines.append("")

    def emit_special(name: str, keys: list[int], offs: list[int], lens: list[int], vals: list[int]):
        out_lines.append(f"pub const {name}_cases = [_]SpecialCase{{")
        for k, o, l in zip(keys, offs, lens):
            out_lines.append(f"    .{{ .key = {zig_u32(k)}, .offset = {zig_u32(o)}, .len = {zig_u32(l)} }},")
        out_lines.append("};")
        out_lines.append("")
        out_lines.append(format_zig_array(f"{name}_values", "u16", [zig_u16(v) for v in vals], columns=12))
        out_lines.append("")

    emit_special("lower_full", lower_keys, lower_offs, lower_lens, lower_vals)
    emit_special("upper_full", upper_keys, upper_offs, upper_lens, upper_vals)

    content = "\n".join(out_lines).rstrip() + "\n"
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(content, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
