from pypdf import PdfWriter, PdfReader
import json
import sys

input_path = sys.argv[1]
output_path = sys.argv[2]

# Get ready to write our PDF
reader = PdfReader(input_path)
writer = PdfWriter(clone_from=reader)

# Receive desired page heights
page_heights = json.load(sys.stdin)
assert len(page_heights) == len(reader.pages)

# Crop each page
for page, height in zip(writer.pages, page_heights):
    page.mediabox.lower_right = (
        page.mediabox.right,
        max(0, page.mediabox.top - (height * 72) - 72 / 2),
    )

# Write the cropped PDF
with open(output_path, "wb") as fp:
    writer.write(fp)
