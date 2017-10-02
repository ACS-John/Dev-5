00010 ! Replace S:\Core\Print\Hdr.br
00020   def library fnhdr(mat hdr$; mat hdrline$)
00030 ! mat hdr$ - a (number of lines,3) dimed array for the top lines of !:
          ! the header in 3 columns
00040 ! mat hdrline$ - an unlimted length array that is for column headings
00050     pr #255,using "Form Pos 1,C 20,Cc 40,Cr 20": hdr$(1,1)(1:20), "{\b "&trim$(hdr$(1,2))&"}", trim$(hdr$(1,3))(1:20)
00060     pr #255,using "Form Pos 1,C 20,Cc 40,Cr 20": hdr$(2,1)(1:20), "{\b "&trim$(hdr$(2,2))&"}", trim$(hdr$(2,3))(1:20)
00070     for j=3 to udim(hdr$,1)
00080       pr #255,using "Form Pos 1,C 20,Cc 40,Cr 20": hdr$(j,1)(1:20), trim$(hdr$(j,2))(1:40), trim$(hdr$(j,3))(1:20)
00090     next j
00100     pr #255: ""
00110     for j=1 to udim(hdrline$)
00120       pr #255: hdrline$(j)
00130     next j
00140   fnend 
