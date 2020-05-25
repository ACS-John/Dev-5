! Replace Test\Grid
	autoLibrary
	test_date= 20082214 ! 20070713  ! date('ccyymmdd')
	mat colhdr$(6)
	colhdr$(1)="ccyymmdd"
	colhdr$(2)="mm/dd/ccyy"
	colhdr$(3)="dd/mm/ccyy"
	colhdr$(4)="dd/mm/ccyy"
	colhdr$(5)="dd/mm/yy"
	colhdr$(5)="source format"
	mat colmask(6)
	colmask$(1)="1"
	colmask$(2)="2"
	colmask$(3)="3"
	colmask$(4)="4"
	colmask$(5)="5"
	colmask$(6)=""
	fnTos(sn$="testflex")
	fnflexinit1("ubrate",1,1,10,50,mat colhdr$,mat colmask$,1)
	dim source_format$(6)
	source_format$(1)='ccyymmdd'
	source_format$(2)='mmddccyy'
	source_format$(3)='mm/dd/ccyy'
	source_format$(4)='ccyy/mm/dd'
	source_format$(5)='mmddyy'
	source_format$(6)='mm/dd/yy'
	for x=1 to udim(mat source_format$)
		dim item$(6)*30
		item$(1)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! date$("mm/dd/ccyy")
		item$(2)=date$(days(test_date,'ccyymmdd'),source_format$(x)) !  date$("ccyy/mm/dd")
		item$(3)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! date$("dd/mm/ccyy")
		item$(4)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! )date$("dd/mm/yy")
		item$(5)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! )date$("dd/mm/yy")
		item$(6)=source_format$(x) ! )date$("dd/mm/yy")
		fnflexadd1(mat item$)
	next x
EO_GRID: !
	fnCmdKey("End",5,0,0)
! ____
	fnAcs2(mat resp$,ckey) ! CALL FLEXGRID
