10000 ! Replace Test\Grid
10040   library 'S:\Core\Library': fncmdkey,fnacs,fnflexinit1,fntos,fnflexadd1
10060   let test_date= 20082214 ! 20070713  ! date('ccyymmdd')
10180   mat colhdr$(6)
10200   let colhdr$(1)="ccyymmdd"
10220   let colhdr$(2)="mm/dd/ccyy"
10240   let colhdr$(3)="dd/mm/ccyy"
10260   let colhdr$(4)="dd/mm/ccyy"
10280   let colhdr$(5)="dd/mm/yy"
10282   let colhdr$(5)="source format"
10300   mat colmask(6)
10320   let colmask$(1)="1"
10340   let colmask$(2)="2"
10360   let colmask$(3)="3"
10380   let colmask$(4)="4"
10400   let colmask$(5)="5"
10402   let colmask$(6)=""
20000   let fntos(sn$="testflex")
20020   let fnflexinit1("ubrate",1,1,10,50,mat colhdr$,mat colmask$,1)
20040   dim source_format$(6)
20060   let source_format$(1)='ccyymmdd'
20080   let source_format$(2)='mmddccyy'
20100   let source_format$(3)='mm/dd/ccyy'
20120   let source_format$(4)='ccyy/mm/dd'
20122   let source_format$(5)='mmddyy'
20124   let source_format$(6)='mm/dd/yy'
20140   for x=1 to udim(mat source_format$)
20160     dim item$(6)*30
20180     let item$(1)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! date$("mm/dd/ccyy")
20200     let item$(2)=date$(days(test_date,'ccyymmdd'),source_format$(x)) !  date$("ccyy/mm/dd")
20220     let item$(3)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! date$("dd/mm/ccyy")
20240     let item$(4)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! )date$("dd/mm/yy")
20260     let item$(5)=date$(days(test_date,'ccyymmdd'),source_format$(x)) ! )date$("dd/mm/yy")
20280     let item$(6)=source_format$(x) ! )date$("dd/mm/yy")
20300     let fnflexadd1(mat item$)
20320   next x
20340 EO_GRID: ! 
20360   let fncmdkey("End",5,0,0)
20380 ! ____
20400   let fnacs(sn$,0,mat resp$,ckey) ! CALL FLEXGRID
