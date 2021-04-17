#install.packages("tesseract")
library(tesseract)

setwd("C:/Code/OpenDataMInfo_TeamMarhack/")

# English (default): PNG to Text
# tesseract engine Defaults to English ('eng')!
eng <- tesseract("eng")
textENpng <- ocr("./Data/TimoBollMaLong2017WC/image4.png", engine = eng)
cat(textENpng)
# write.table(textENpng, "./results/sampleEN2.txt")

# English (default): PDF to PNG to Text
eng <- tesseract("eng")
origENpdf <- pdftools::pdf_text("./samples/sampleEN1.pdf")[1]
pdf2pngfileEN <- pdftools::pdf_convert('./samples/sampleEN1.pdf', format = 'tiff', pages = 1, dpi = 800)
textENpdf <- tesseract::ocr(pdf2pngfileEN, engine = eng)
cat(textENpdf)
# write.table(textENpdf, "./results/sampleEN1.txt")

# multi-page at once
deu <- tesseract("deu")
origDEpdfm <- pdftools::pdf_text("./samples/sampleDE.pdf")
pdf2pngfileDEm <- pdftools::pdf_convert('./samples/sampleDE.pdf', format = 'tiff', dpi = 800)
textDEpdfm <- tesseract::ocr(pdf2pngfileDEm, engine = deu)
cat(textDEpdfm)
# write.table(textDEpdfm, "./results/sampleDE.txt")