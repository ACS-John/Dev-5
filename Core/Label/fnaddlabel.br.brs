00010 ! Replace S:\Core\Label\fnAddLabel
00020 ! add a label to a queue that will be printed when fnLabel is called
00030 ! ______________________________________________________________________
00040   def library fnaddlabel(mat in_labeltext$)
00050 ! ______________________________________________________________________
00060     library 'S:\Core\Library': fnerror,fngethandle
00070     on error goto ERTN
00080 ! ______________________________________________________________________
00090     dim labeltext$(5)*120
00100 ! ______________________________________________________________________
00111     if udim(in_labeltext$)<>5 then !:
            pr "fnAddLabel - You should only send 5 array items to fnAddLabel."
00120     for j=1 to min(5,udim(in_labeltext$)) !:
            let labeltext$(j)=in_labeltext$(j)(1:min(len(in_labeltext$(j)),120)) !:
          next j
00130     open #tmp:=fngethandle: "Name="&env$('temp')&"\Label.dat,RecL=600,Use",internal,output  !:
          write #tmp,using "Form POS 1,5*C 120": mat labeltext$ !:
          close #tmp: 
00140     mat labeltext$=("")
00150     goto XIT
00160 ! ______________________________________________________________________
00170 ! <Updateable Region: ERTN>
00180 ERTN: let fnerror(program$,err,line,act$,"xit")
00190     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00200     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00210     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00220 ERTN_EXEC_ACT: execute act$ : goto ERTN
00230 ! /region
00240 ! ______________________________________________________________________
00250 XIT: fnend 
00260 ! ______________________________________________________________________
