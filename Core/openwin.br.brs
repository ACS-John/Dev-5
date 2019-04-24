! Replace S:\Core\OpenWin.br
! opens a console window w/ company name and number at the top
! very old
def library fnopenwin(win,sr,sc,er,ec,&cap$)
	if sr<1 then sr=10
	if sc<1 then sc=20
	if er<1 then er=14
	if ec<1 then ec=59
	win_width=ec-sc+1
	close #win: ioerr ignore
	open #win: 'SRow='&str$(sr)&',SCol='&str$(sc)&',ERow='&str$(er)&',ECol='&str$(ec)&',Border=Sr,Caption=<'&cap$,display,outIn 
	pr #win: newpage
	pr #win,fields '1,1,Cc '&str$(win_width)&',R,N': env$('cnam')(1:min(40,win_width))
	pr #win,fields '2,1,Cc '&str$(win_width)&',R,N': 'Company Number [cno]'(1:min(40,win_width))
fnend
