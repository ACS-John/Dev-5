00010 ! Replace S:\Core\Parse\GetPP.br
00020 ! Get Path and Program (v1.01) !:
        ! rev 1.01 - changes to a shorter file name from S:\Core\getpathprog.br !:
        ! to S:\Core\getpp.br
00030   def library fngetpp(&input$,&path$,&prog$,&ext$)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fnerror
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080 ! Dim Note: Please Dim you Path$ and Prog$ as long as your Input$
00090 ! Input$: this is what you want parsed... !:
          !         supported formats:  progam/dir  !:
          !                             program.ext/dir  !:
          !                             dir\program !:
          !                             dir\program.ext !:
          !                             dir\dir\program.ext
00100 ! path$:  the path to the input program (i.e. "S:\acsUB\")
00110 ! prog$:  the file name (without it's extension) (i.e. "ubmenu")
00120 ! ext$:   the input progams extension (period included) (i.e. ".wb")
00130 ! The -1 in pos(x,y,-1) causes search to run backwards from end to front
00140 ! ______________________________________________________________________
00150     let input$=trim$(input$) : let path$=prog$=ext$=""
00160     let fslash_pos=pos(input$,"/",1) : let bslash_pos=pos(input$,"\",-1)
00170     if fslash_pos>0 then gosub FSLASH_PARSE
00180     if bslash_pos>0 then gosub BSLASH_PARSE
00190     if fslash_pos<=0 and bslash_pos<=0 then gosub NOSLASH_PARSE
00200     let dot_pos=pos(prog$,".",-1)
00210     if dot_pos>0 then gosub RIP_EXT
00220     goto XIT
00230 ! ______________________________________________________________________
00240 FSLASH_PARSE: ! front slash parse !:
          let prog$=input$(1:fslash_pos-1) !:
          let path$=input$(fslash_pos+1:len(input$))
00250     return 
00260 ! ______________________________________________________________________
00270 BSLASH_PARSE: ! Back slash parse !:
          let prog$=input$(bslash_pos+1:len(input$)) !:
          let path$=input$(1:bslash_pos)
00280     return 
00290 ! ______________________________________________________________________
00300 NOSLASH_PARSE: ! No slash parse !:
          let prog$=input$(1:len(input$)) : let path$=""
00310     return 
00320 ! ______________________________________________________________________
00330 RIP_EXT: ! Extract Ext$ from Prog$ !:
          let ext$=prog$(dot_pos:len(prog$)) : let prog$=prog$(1:dot_pos-1)
00340     return 
00350 ! ______________________________________________________________________
00360 ! <Updateable Region: ERTN>
00370 ERTN: let fnerror(program$,err,line,act$,"xit")
00380     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00390     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00400     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00410 ERTN_EXEC_ACT: execute act$ : goto ERTN
00420 ! /region
00430 ! ______________________________________________________________________
00440 XIT: ! 
00441     let path$=trim$(path$)
00450     if path$(len(path$):len(path$))<>"\" then let path$=trim$(path$)&"\"
00470   fnend 
00480 ! ______________________________________________________________________
