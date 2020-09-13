def library fnHdr(mat hdr$; mat hdrline$)
	! mat hdr$ - a (number of lines,3) dimed array for the top lines of 
	! the header in 3 columns
	! mat hdrline$ - an unlimted length array that is for column headings
	pr #255,using "Form Pos 1,C 20,Cc 40,Cr 20": hdr$(1,1)(1:20), "{\b "&trim$(hdr$(1,2))&"}", trim$(hdr$(1,3))(1:20)
	pr #255,using "Form Pos 1,C 20,Cc 40,Cr 20": hdr$(2,1)(1:20), "{\b "&trim$(hdr$(2,2))&"}", trim$(hdr$(2,3))(1:20)
	for j=3 to udim(hdr$,1)
		pr #255,using "Form Pos 1,C 20,Cc 40,Cr 20": hdr$(j,1)(1:20), trim$(hdr$(j,2))(1:40), trim$(hdr$(j,3))(1:20)
	next j
	pr #255: ""
	for j=1 to udim(hdrline$)
		pr #255: hdrline$(j)
	next j
fnend 
