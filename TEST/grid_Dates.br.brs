 pr newpage
! r: set header stuff
  dim gridspec$*256
  gridspec$='#0,6,3,list 20/70'
  dim _headings$(6)*256
  _headings$(1)='Combined'
  _headings$(2)='Rec'
  _headings$(3)='Reference #'
  _headings$(4)='Date'
  _headings$(5)='Amount'
  _headings$(6)='Cleared'
  dim _widths(6)
  _widths(1)=0
  _widths(2)=7
  _widths(3)=15
  _widths(4)=8
  _widths(5)=10
  _widths(6)=11
  dim _forms$(6)*256
  _forms$(1)='0/C 500'
  _forms$(2)='7/#PIC(-----------------)'
  _forms$(3)='C 15'
  _forms$(4)='8/date(m/d/yy)'
  _forms$(5)='10/#PIC(-,---,---,---,---.--)'
  _forms$(6)='11/date(m/d/yy)'
! /r
! r: set mat long row
  dim long_row$(60)*256
  long_row$(1 )='1  21    3323 112610 3.91      0'
  long_row$(2 )='1'
  long_row$(3 )=' 21    3323'
  long_row$(4 )='40507'
  long_row$(5 )='3.91'
  long_row$(6 )='0'

  long_row$(7 )='2  21    3467  72211 25.13      0'
  long_row$(8 )='2'
  long_row$(9 )=' 21    3467'
  long_row$(10)='40745'
  long_row$(11)='25.13'
  long_row$(12)='0'

  long_row$(13)='3  21    3470  72211 50.72      0'
  long_row$(14)='3'
  long_row$(15)=' 21    3470'
  long_row$(16)='40745'
  long_row$(17)='50.72'
  long_row$(18)='0'

  long_row$(19)='4  21    3509  91611 2.33      0'
  long_row$(20)='4'
  long_row$(21)=' 21    3509'
  long_row$(22)='40801'
  long_row$(23)='2.33'
  long_row$(24)='0'

  long_row$(25)='5  21    3510  91611 27.44      0'
  long_row$(26)='5'
  long_row$(27)=' 21    3510'
  long_row$(28)='40801'
  long_row$(29)='27.44'
  long_row$(30)='0'

  long_row$(31)='6  21    3515  93011 1.14      0'
  long_row$(32)='6'
  long_row$(33)=' 21    3515'
  long_row$(34)='40815'
  long_row$(35)='1.14'
  long_row$(36)='0'

  long_row$(37)='7  21    3530 101411 4.93      0'
  long_row$(38)='7'
  long_row$(39)=' 21    3530'
  long_row$(40)='40829'
  long_row$(41)='4.93'
  long_row$(42)='0'

  long_row$(43)='8  21    3531 101411 3.24      0'
  long_row$(44)='8'
  long_row$(45)=' 21    3531'
  long_row$(46)='40829'
  long_row$(47)='3.24'
  long_row$(48)='0'

  long_row$(49)='9  21    3537 101411 50.09      0'
  long_row$(50)='9'
  long_row$(51)=' 21    3537'
  long_row$(52)='40829'
  long_row$(53)='50.09'
  long_row$(54)='0'
! 
  long_row$(55)='10  21    3576 120911 6.01      0'
  long_row$(56)='10'
  long_row$(57)=' 21    3576'
  long_row$(58)='40885'
  long_row$(59)='6.01'
  long_row$(60)='0'
! /r
  dim ace_io$(4)*256
  ace_io$(1)='#0,6,3,list 20/70,row,selone'
  ace_io$(2)='#0,5,3,70/filter 71,[textboxes],6,3,1,word'
  ace_io$(3)='#0,2,49,12/#PIC(--------.--),T[textboxes],300'
  ace_io$(4)='#0,3,48,check 2,T'
  dim ace_resp$(3)*256
  ace_resp$(1)=''
  ace_resp$(2)='0'
  ace_resp$(3)='^'
! 
  pr f gridspec$&",headers,[gridheaders]" : (mat _headings$,mat _widths,mat _forms$)
  pr f gridspec$&",=L": mat long_row$ ! (1:(row_count)*udim(_chunks$))
  dim grid_filter$*256,grid_row$(6)*256
  rinput fields mat ace_io$: mat grid_row$, grid_filter$, mat ace_resp$(2:udim(ace_resp$))
