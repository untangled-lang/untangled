from pypdf import PdfWriter, PdfReader
import sys

if len(sys.argv) < 3:
    print("Usage: python merge-pdfs.py input1.pdf input2.pdf ... output.pdf")
    sys.exit(1)

input_paths = sys.argv[1:-1]
output_path = sys.argv[-1]

writer = PdfWriter()

for input_path in input_paths:
    # Get ready to write our PDF
    reader = PdfReader(input_path)
    writer.append(reader)

# Write the cropped PDF
with open(output_path, "wb") as fp:
    writer.write(fp)
