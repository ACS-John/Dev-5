10000   print newpage
20000 ! r: set header stuff
20020   dim gridspec$*256
20040   let gridspec$='#0,6,3,list 20/70'
20060   dim _headings$(6)*256
20080   let _headings$(1)='Combined'
20100   let _headings$(2)='Rec'
20120   let _headings$(3)='Reference #'
20140   let _headings$(4)='Date'
20160   let _headings$(5)='Amount'
20180   let _headings$(6)='Cleared'
20200   dim _widths(6)
20220   let _widths(1)=0
20240   let _widths(2)=7
20260   let _widths(3)=15
20280   let _widths(4)=8
20300   let _widths(5)=10
20320   let _widths(6)=11
20340   dim _forms$(6)*256
20360   let _forms$(1)='0/C 500'
20380   let _forms$(2)='7/#PIC(-----------------)'
20400   let _forms$(3)='C 15'
20420   let _forms$(4)='8/date(m/d/yy)'
20440   let _forms$(5)='10/#PIC(-,---,---,---,---.--)'
20460   let _forms$(6)='11/date(m/d/yy)'
20480 ! /r
20500 ! r: set mat long row
20520   dim long_row$(60)*256
20540   let long_row$(1 )='1  21    3323 112610 3.91      0'
20560   let long_row$(2 )='1'
20580   let long_row$(3 )=' 21    3323'
20600   let long_row$(4 )='40507'
20620   let long_row$(5 )='3.91'
20640   let long_row$(6 )='0'
20660 ! 
20680   let long_row$(7 )='2  21    3467  72211 25.13      0'
20700   let long_row$(8 )='2'
20720   let long_row$(9 )=' 21    3467'
20740   let long_row$(10)='40745'
20760   let long_row$(11)='25.13'
20780   let long_row$(12)='0'
20800 ! 
20820   let long_row$(13)='3  21    3470  72211 50.72      0'
20840   let long_row$(14)='3'
20860   let long_row$(15)=' 21    3470'
20880   let long_row$(16)='40745'
20900   let long_row$(17)='50.72'
20920   let long_row$(18)='0'
20940 ! 
20960   let long_row$(19)='4  21    3509  91611 2.33      0'
20980   let long_row$(20)='4'
21000   let long_row$(21)=' 21    3509'
21020   let long_row$(22)='40801'
21040   let long_row$(23)='2.33'
21060   let long_row$(24)='0'
21080 ! 
21100   let long_row$(25)='5  21    3510  91611 27.44      0'
21120   let long_row$(26)='5'
21140   let long_row$(27)=' 21    3510'
21160   let long_row$(28)='40801'
21180   let long_row$(29)='27.44'
21200   let long_row$(30)='0'
21220 ! 
21240   let long_row$(31)='6  21    3515  93011 1.14      0'
21260   let long_row$(32)='6'
21280   let long_row$(33)=' 21    3515'
21300   let long_row$(34)='40815'
21320   let long_row$(35)='1.14'
21340   let long_row$(36)='0'
21360 ! 
21380   let long_row$(37)='7  21    3530 101411 4.93      0'
21400   let long_row$(38)='7'
21420   let long_row$(39)=' 21    3530'
21440   let long_row$(40)='40829'
21460   let long_row$(41)='4.93'
21480   let long_row$(42)='0'
21500 ! 
21520   let long_row$(43)='8  21    3531 101411 3.24      0'
21540   let long_row$(44)='8'
21560   let long_row$(45)=' 21    3531'
21580   let long_row$(46)='40829'
21600   let long_row$(47)='3.24'
21620   let long_row$(48)='0'
21640 ! 
21660   let long_row$(49)='9  21    3537 101411 50.09      0'
21680   let long_row$(50)='9'
21700   let long_row$(51)=' 21    3537'
21720   let long_row$(52)='40829'
21740   let long_row$(53)='50.09'
21760   let long_row$(54)='0'
21780 ! 
21800   let long_row$(55)='10  21    3576 120911 6.01      0'
21820   let long_row$(56)='10'
21840   let long_row$(57)=' 21    3576'
21860   let long_row$(58)='40885'
21880   let long_row$(59)='6.01'
21900   let long_row$(60)='0'
21940 ! /r
21960   dim ace_io$(4)*256
22000   let ace_io$(1)='#0,6,3,list 20/70,row,selone'
22040   let ace_io$(2)='#0,5,3,70/filter 71,[textboxes],6,3,1,word'
22080   let ace_io$(3)='#0,2,49,12/#PIC(--------.--),T[textboxes],300'
22120   let ace_io$(4)='#0,3,48,check 2,T'
22140   dim ace_resp$(3)*256
22160   let ace_resp$(1)=''
22180   let ace_resp$(2)='0'
22200   let ace_resp$(3)='^'
22220 ! 
22240   print fields gridspec$&",headers,[gridheaders]" : (mat _headings$,mat _widths,mat _forms$)
22260   print fields gridspec$&",=L": mat long_row$ ! (1:(row_count)*udim(_chunks$))
22280   dim grid_filter$*256,grid_row$(6)*256
22300   rinput fields mat ace_io$: mat grid_row$, grid_filter$, mat ace_resp$(2:udim(ace_resp$))
