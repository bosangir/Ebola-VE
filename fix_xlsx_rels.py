#!/usr/bin/env python3
"""Strip dangling relationships (drawing / vmlDrawing / printerSettings that were
never written) that this openxlsx build injects into every worksheet, and remove
the matching <drawing>/<legacyDrawing>/pageSetup r:id references from the sheet
XML, so the workbook validates cleanly (no Excel 'repair' prompt)."""
import sys, re, zipfile, shutil, os
src = sys.argv[1]
names = None
with zipfile.ZipFile(src) as z:
    names = set(z.namelist())
    parts = {n: z.read(n) for n in z.namelist()}

def resolve(target):  # resolve ../drawings/x.xml relative to xl/worksheets/
    t = target
    base = "xl/worksheets/"
    while t.startswith("../"):
        t = t[3:]; base = base.rsplit("/", 2)[0] + "/"
    return (base + t).replace("xl/xl/", "xl/")

removed_total = 0
for name in list(parts):
    m = re.match(r"xl/worksheets/_rels/(sheet\d+\.xml)\.rels$", name)
    if not m:
        continue
    sheet_xml = "xl/worksheets/" + m.group(1)
    txt = parts[name].decode("utf-8")
    rels = re.findall(r'<Relationship\b[^>]*/>', txt)
    drop_ids = []
    for r in rels:
        tgt = re.search(r'Target="([^"]+)"', r)
        rid = re.search(r'Id="([^"]+)"', r)
        if not tgt or not rid:
            continue
        resolved = resolve(tgt.group(1))
        if resolved not in names:      # part does not exist -> dangling
            drop_ids.append(rid.group(1))
            txt = txt.replace(r, "")
            removed_total += 1
    parts[name] = txt.encode("utf-8")
    # strip references in the sheet xml
    sx = parts[sheet_xml].decode("utf-8")
    for rid in drop_ids:
        sx = re.sub(r'<drawing[^>]*r:id="%s"[^>]*/>' % re.escape(rid), "", sx)
        sx = re.sub(r'<legacyDrawing[^>]*r:id="%s"[^>]*/>' % re.escape(rid), "", sx)
        sx = re.sub(r'(<pageSetup\b[^>]*?)\s+r:id="%s"' % re.escape(rid), r"\1", sx)
    # remove any now-orphaned self-closing drawing/legacyDrawing with no matching part
    sx = re.sub(r'<drawing[^>]*/>', "", sx)
    sx = re.sub(r'<legacyDrawing[^>]*/>', "", sx)
    parts[sheet_xml] = sx.encode("utf-8")

tmp = src + ".tmp"
with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as z:
    for n, b in parts.items():
        z.writestr(n, b)
shutil.move(tmp, src)
print("fixed dangling relationships removed:", removed_total)
