20000 ! Replace S:\Core\Parse\GetPP.br
20020 ! Get Path and Program (v1.02)
20040 ! rev 1.01 - changes to a shorter file name from S:\Core\getpathprog.br
20040 ! rev 1.02 - clean up code
20060 ! to S:\Core\getpp.br
20080 def library fnGetPp(&input$,&path$,&prog$,&ext$)
20100   library 'S:\Core\Library': fnerror
20120   on error goto ERTN
20140   ! ______________________________________________________________________
20160   ! Dim Note: Please Dim you Path$ and Prog$ as long as your Input$
20180   ! Input$: this is what you want parsed...
20200   !         supported formats:  progam/dir 
20220   !                             program.ext/dir 
20240   !                             dir\program
20260   !                             dir\program.ext
20280   !                             dir\dir\program.ext
20300   ! path$:  the path to the input program (i.e. "S:\acsUB\")
20320   ! prog$:  the file name (without it's extension) (i.e. "ubmenu")
20340   ! ext$:   the input progams extension (period included) (i.e. ".wb")
20360   ! The -1 in pos(x,y,-1) causes search to run backwards from end to front
20380   ! ______________________________________________________________________
20400   input$=trim$(input$) : path$=prog$=ext$=""
20420   fslash_pos=pos(input$,"/",1) : bslash_pos=pos(input$,"\",-1)
20440   if fslash_pos>0 then gosub FSLASH_PARSE
20460   if bslash_pos>0 then gosub BSLASH_PARSE
20480   if fslash_pos<=0 and bslash_pos<=0 then gosub NOSLASH_PARSE
20500   dot_pos=pos(prog$,".",-1)
20520   if dot_pos>0 then gosub RIP_EXT
20540   goto XIT
20560   ! 
20580   FSLASH_PARSE: ! r: front slash parse 
20600     prog$=input$(1:fslash_pos-1) 
20620     path$=input$(fslash_pos+1:len(input$))
20640   return ! /r
20660   BSLASH_PARSE: ! r: Back slash parse 
20680     prog$=input$(bslash_pos+1:len(input$)) 
20700     path$=input$(1:bslash_pos)
20720   return ! /r
20740   NOSLASH_PARSE: ! r: No slash parse 
20760     prog$=input$(1:len(input$)) 
20780     path$=""
20800   return ! /r
20820   RIP_EXT: ! r: Extract Ext$ from Prog$
20840     ext$=prog$(dot_pos:len(prog$))
20860     prog$=prog$(1:dot_pos-1)
20880   return ! /r
21040   XIT: ! 
21060   path$=trim$(path$)
21080   if path$(len(path$):len(path$))<>"\" then path$=trim$(path$)&"\"
21100 fnend 
20900   ! <Updateable Region: ERTN>
20920   ERTN: fnerror(program$,err,line,act$,"xit")
20940     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
20960     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20980     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
21000   ERTN_EXEC_ACT: execute act$ : goto ERTN
21020   ! /region