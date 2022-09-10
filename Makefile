### Makefile for [Project Title]       ###

PAPER = [file_name for md/LaTeX file]
TEXT   = ./paper
FIG    = ./figures
TAB    = ./tables
CODE   = ./code
RAW    = ./raw_data
DAT    = ./data

### To run on Mac and Linux
### If you want to run this on Windows you will have to figure out how to call a pdf viewer
OS := $(shell uname)
ifeq ($(OS),Darwin)
PDFAPP := open -a Skim
else
PDFAPP := evince 
endif
