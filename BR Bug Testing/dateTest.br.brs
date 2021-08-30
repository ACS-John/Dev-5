20000   print newpage
21000   open #0: 'srow=1,scol=1,Rows=37,Cols=116',d,o 
22000   print #0,fields '2,2,C': 'Tab, tab'
22020   print #0,fields '3,2,C': 'right arrow, hold shift, left arrow, left arrow, release shift, 7, tab'
22040   print #0,fields '4,2,C': 'right arrow, hold shift, left arrow, left arrow, release shift, 7, tab'
22060   print #0,fields '5,2,C': 'you should encounter the brassertion (ret <= datalen) failed in'
22080   print #0,fields '6,2,C': 'c:\cugwin64\home\gordo_000\br43\br\brs\brcontrols.h line 359. ...'
30000   open #188: 'srow=11,scol=24,erow=24,ecol=92,border=S:[screen],N=[screen],caption=Change Payroll Date',d,o 
30040   open #187: 'srow=27,scol=24,erow=27,ecol=92,border=S:[screen],N=[screen]',d,o 
30060   open #186: 'srow=2,scol=2,erow=5,ecol=67,parent=188,border=S:[screen],N=[screen],caption=Payroll Date',d,o 
30080   open #185: 'srow=8,scol=26,erow=13,ecol=67,parent=188,border=S:[screen],N=[screen],caption=Date Range',d,o 
30120   dim ace_io$(8)*256
30140   ace_io$(1)='#186,1,45,9/#PIC(--/--/--),T[textboxes],300'
30160   ace_io$(2)='#186,2,45,20/C 20,T[textboxes],300'
30180   ace_io$(3)='#185,1,28,9/#PIC(--/--/--),T[textboxes],300'
30200   ace_io$(4)='#185,2,28,9/#PIC(--/--/--),T[textboxes],300'
30220   ace_io$(5)='#185,3,28,9/#PIC(--/--/--),T[textboxes],300'
30240   ace_io$(6)='#185,4,28,9/#PIC(--/--/--),T[textboxes],300'
30260   ace_io$(7)='#185,5,28,9/#PIC(--/--/--),T[textboxes],300'
30280   ace_io$(8)='#185,6,28,9/#PIC(--/--/--),T[textboxes],300'
30420   dim ace_resp$(8)*256
30440   ace_resp$(1)='020516'
30460   ace_resp$(2)='February 5, 2016'
30480   ace_resp$(3)='010116'
30500   ace_resp$(4)='123116'
30520   ace_resp$(5)='010116'
30540   ace_resp$(6)='040116'
30560   ace_resp$(7)='070116'
30580   ace_resp$(8)='100116'
30600   rinput fields mat ace_io$: mat ace_resp$ ! conv CONV_HANDLER
