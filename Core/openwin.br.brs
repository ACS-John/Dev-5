00010 ! Replace S:\Core\OpenWin.br
00020 ! opens a console window w/ company name and number at the top !:
        ! very old - probably not even used anymore
00030 ! ______________________________________________________________________
00040   def library fnopenwin(win,sr,sc,er,ec,&cap$)
00050 ! ______________________________________________________________________
00080     library 'S:\Core\Library': fnerror
00090     on error goto ERTN
00100 ! ______________________________________________________________________
00120     if sr<1 then sr=10
00130     if sc<1 then sc=20
00140     if er<1 then er=14
00150     if ec<1 then ec=59
00160     win_width=ec-sc+1
00170     close #win: ioerr L180
00180 L180: open #win: 'SRow='&str$(sr)&',SCol='&str$(sc)&',ERow='&str$(er)&',ECol='&str$(ec)&',Border=Sr,Caption=<'&cap$,display,outin 
00190     pr #win: newpage
00200     pr #win,fields '1,1,Cc '&str$(win_width)&',R,N': env$('cnam')(1:min(40,win_width))
00210     pr #win,fields '2,1,Cc '&str$(win_width)&',R,N': 'Company Number '&env$('cno')(1:min(40,win_width))
00220     goto XIT
00230 ! ______________________________________________________________________
00240 ! <Updateable Region: ERTN>
00250 ERTN: fnerror(program$,err,line,act$,'xit')
00260     if lwrc$(act$)<>'pause' then goto ERTN_EXEC_ACT
00270     execute 'List -'&str$(line) : pause : goto ERTN_EXEC_ACT
00280     pr 'PROGRAM PAUSE: Type GO and press [Enter] to continue.' : pr '' : pause : goto ERTN_EXEC_ACT
00290 ERTN_EXEC_ACT: execute act$ : goto ERTN
00300 ! /region
00310 ! ______________________________________________________________________
00320 XIT: fnend 
00330 ! ______________________________________________________________________
