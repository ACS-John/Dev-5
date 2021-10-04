open #0:   'srow= 1, scol= 1, rows=38, cols=116',d,o
open #183: 'srow=11, scol=27, erow=24, ecol= 90',d,o
open #182: 'srow=27, scol=27, erow=27, ecol= 90',d,o
dim ace_io$(19)*256
ace_io$(1 )='#183,1,32,10/CR 10,P[textboxes],300'
ace_io$(2 )='#183,2,32,30/C 30,P[textboxes],300'
ace_io$(3 )='#183,3,32,10/#PIC(--------.--),P[textboxes],300'
ace_io$(4 )='#183,7,44,12/#PIC(--------.--),P[textboxes],300'
ace_io$(5 )='#183,7,32,12/#PIC(--------.--),T[textboxes],300'
ace_io$(6 )='#183,8,44,12/#PIC(--------.--),P[textboxes],300'
ace_io$(7 )='#183,8,32,12/#PIC(--------.--),T[textboxes],300'
ace_io$(8 )='#183,9,44,12/#PIC(--------.--),P[textboxes],300'
ace_io$(9 )='#183,9,32,12/#PIC(--------.--),T[textboxes],300'
ace_io$(10)='#183,10,44,12/#PIC(--------.--),P[textboxes],300'
ace_io$(11)='#183,10,32,12/#PIC(--------.--),T[textboxes],300'
ace_io$(12)='#183,11,44,12/#PIC(--------.--),P[textboxes],300'
ace_io$(13)='#183,11,32,12/#PIC(--------.--),T[textboxes],300'
ace_io$(14)='#183,12,44,12/#PIC(--------.--),P[textboxes],300'
ace_io$(15)='#183,12,32,12/#PIC(--------.--),T[textboxes],300'
ace_io$(16)='#183,13,44,12/#PIC(--------.--),P[textboxes],300'
ace_io$(17)='#183,13,32,12/#PIC(--------.--),T[textboxes],300'
ace_io$(18)='#183,14,44,12/#PIC(--------.--),P[textboxes],300'
ace_io$(19)='#183,14,32,12/#PIC(--------.--),T[textboxes],300'
dim ace_resp$(19)*256
ace_resp$(1 )='1000000.01'
ace_resp$(2 )='JAMES WALTON'
ace_resp$(3 )='75'
ace_resp$(4 )='21.25'
ace_resp$(5 )='27.55'
ace_resp$(6 )='20.45'
ace_resp$(7 )='20.45'
ace_resp$(8 )='0'
ace_resp$(9 )='0'
ace_resp$(10)='25'
ace_resp$(11)='25'
ace_resp$(12)='2'
ace_resp$(13)='2'
ace_resp$(14)='-68.7'
ace_resp$(15)='0'
ace_resp$(16)='0'
ace_resp$(17)='0'
ace_resp$(18)='0'
ace_resp$(19)='0'

rinput fields mat ace_io$: mat ace_resp$ conv CONV_HANDLER


CONV_HANDLER: ! r:
  dim temp_io$(1)*255
  bad_field=cnt+1
  mat temp_io$(udim(ace_io$))=ace_io$
  msgbox('You have entered an incorrect value at field number '&str$(bad_field), "Error!")
  ! pause
  mat ace_resp$(udim(resp$))=resp$
  mat ace_io$(udim(temp_io$))=temp_io$
  curfld(bad_field)
retry  ! /r