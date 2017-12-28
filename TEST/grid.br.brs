10000 ! Replace Test\Grid
10040   library 'S:\Core\Library': fnCmdKey,fnAcs,fnflexinit1,fnTos,fnflexadd1
10060   test_date= 20082214 ! 20070713  ! date('ccyymmdd')
10180   mat colhdr$(6)
10200   colhdr$(1)="ccyymmdd"
10220   colhdr$(2)="mm/dd/ccyy"
10240   colhdr$(3)="dd/mm/ccyy"
10260   colhdr$(4)="dd/mm/ccyy"
10280   colhdr$(5)="dd/mm/yy"
10282   colhdr$(5)="source format"
10300   mat colmask(6)
10320   colmask$(1)="1"
10340   colmask$(2)="2"
10360   colmask$(3)="3"
10380   colmask$(4)="4"
10400   colmask$(5)="5"
10402   colmask$(6)=""
20000   fnTos(sn$="testflex")
20020   fnflexinit1("ubrate",1,1,10,50,mat colhdr$,mat colmask$,1)
20040   dim source_format$(6)
20060   source_format$(1)='ccyymmdd'
20080   source_format$(2)='mmddccyy'
20100   source_format$(3)='mm/dd/ccyy'
20120   source_format$(4)='ccyy/mm/dd'
20122   source_format$(5)='mmddyy'
20124   source_format$(6)='mm/dd/yy'
20140   for x=1 to udim(mat source_format$)
20160     dim item$(6)*30
20180     item$(1)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! date$("mm/dd/ccyy")
20200     item$(2)=date$(days(test_date,'ccyymmdd'),source_format$(x)) !  date$("ccyy/mm/dd")
20220     item$(3)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! date$("dd/mm/ccyy")
20240     item$(4)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! )date$("dd/mm/yy")
20260     item$(5)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! )date$("dd/mm/yy")
20280     item$(6)=source_format$(x) ! )date$("dd/mm/yy")
20300     fnflexadd1(mat item$)
20320   next x
20340 EO_GRID: ! 
20360   fnCmdKey("End",5,0,0)
20380 ! ____
20400   fnAcs(sn$,0,mat resp$,ckey) ! CALL FLEXGRID
