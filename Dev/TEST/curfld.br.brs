dim ace_io$(10)*256,ace_data$(10)*256

open #186: "SROW= 1, SCOL= 1, EROW=35, ECOL=129",d,outIn
open #188: "SROW= 3, SCOL= 2, EROW=31, ECOL=114",display,outIn 
open #187: "SROW=34, SCOL= 2, EROW=34, ECOL=114",display,outIn 
open #186: "SROW= 2, SCOL= 2, EROW= 4, ECOL=40, PARENT=188",display,outIn 
open #185: "SROW= 8, SCOL= 2, EROW=19, ECOL=61, PARENT=188",display,outIn 

dim opt$(5)*28
opt$(1)="0 = Not Finaled"
opt$(2)="1 = Final Bill"
opt$(3)="2 = Final & Refund Deposit"
opt$(4)="3 = Active, but do not Bill"
opt$(5)="4 = Finaled, but not billed"
pr f '#185,7,24,28/combo 128,+,select': mat opt$

!  curfld(4)

ace_io$(01)="#186,1,17,10/C 10,P[textboxes],300"
ace_io$(02)="#186,2,17,30/C 30,P[textboxes],300"
ace_io$(03)="#186,3,17,10/C 10,P[textboxes],300"
ace_io$(04)="#185,2,14,10/#PIC(---,---,---),T[textboxes],300"
ace_io$(05)="#185,2,26,10/#PIC(--,---.--),T[textboxes],300"
ace_io$(06)="#185,2,38,10/#PIC(---,---,---),T[textboxes],300"
ace_io$(07)="#185,3,26,10/#PIC(--,---.--),P[textboxes],300"
ace_io$(08)="#185,4,38,10/#PIC(---,---,---),T[textboxes],300"
ace_io$(09)="#185,5,26,10/#PIC(--,---.--),T[textboxes],300"
ace_io$(10)="#185,7,24,28/combo 128,Select[textboxes]"
! ace_io$(11)="#188,2,65,list 27/49,row,selone"
! ace_io$(12)="#188,1,65,49/filter 47,[textboxes],2,65,1,word"
 
rinput fields mat ace_io$: mat ace_data$
