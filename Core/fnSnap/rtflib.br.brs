00010 ! REPLACE RTFLIB_dll.br
00019   library program$: fnrtf,fnamt$,fntext$ ! library "RTFLIB_dll.br": fnrtf,fnamt$,fntext$
00020   dim types$(4)*2,styles$(4)*1000,data$(20)*1200,sub$(1)*1000
00030   let rtffile=126 !:
        open #rtffile: "name="&env$('temp')&"\temp.rtf,recl=1000,replace",display,output 
00040   let types$(1)="H" !:
        let types$(2)="F" !:
        let types$(3)="D" !:
        let types$(4)="T"
00050   let styles$(1)="li0|ri0|fs18|cfBlue|tc3.25|Header"
00060   let styles$(2)="li0|ri0|fs8|cfRed|tc3.25|Footer"
00070   let styles$(3)="li0.25|ri0|fs10||tl2.5|td3|tl3.2|td4.0|tl4.2|td4.6|tl4.8|td5.4|Data"
00075   let mask$="pic(ZZZ,ZZ#)"
00080   let styles$(4)="li0.5|ri0|fs10|tl2.5|td3|tl3.2|td4.0|tl4.2|td4.6|tl4.8|td5.4|Totals"
00090   let data$(1)="H|\b\tab Title Of Report"
00100   let data$(2)="F|\tab Page Footer"
00105   let data$(3)="D|Description |"&fnamt$(1000,mask$,"$")&"|"&fnamt$(2000,mask$,"$")&"|"&fnamt$(3000,mask$,"$")&"|"&fnamt$(4000,mask$,"$")
00110   for a=4 to 17
00120     if mod(a,2) then let data$(a)="D|Description |"&fnamt$(1000,mask$)&"|"&fnamt$(2000,mask$)&"|"&fnamt$(3000,mask$)&"|"&fnamt$(4000,mask$) else let data$(a)="D|Description |"&fnamt$(-1000,mask$)&"|"&fnamt$(-2000,mask$)&"|"&fnamt$(-3000,mask$)&"|"&fnamt$(-4000,mask$)
00130   next a
00135   let data$(18)="D|"&fntext$("Description that is longer than the allowed size of the data space and needs to be out on multiple lines",30)&"|"&fnamt$(1000,mask$," ","s")&"|"&fnamt$(2000,mask$," ","s")&"|"&fnamt$(3000,mask$," ","s")&"|"&fnamt$(4000,mask$," ","s")
00140   let data$(19)="T|Total|"&fnamt$(18000,mask$,"$","d")&"|"&fnamt$(36000,mask$,"$","d")&"|\cfRed "&fnamt$(54000,mask$,"$","d")&"|"&fnamt$(72000,mask$,"$","d")
00150   let fnrtf(mat types$,mat styles$,mat data$,rtffile)
00157   dim rtffile$*250
00158   let rtffile$=file$(rtffile)
00159   close #rtffile: 
00160   execute "sys "&env$("PD")&"spoolbat.bat "&rtffile$&" WORD"
00170   stop 
00180 INIT: ! 
00199 ! 
00200 ! 
00201 ! 
00210   let datfmt$="MM-DD-CCYY" !:
        let maxsrows=22 !:
        let ssav=103 !:
        let windev=owindev=69 !:
        let mga$="24,2,c 78," !:
        let pfk=23 !:
        ! Common Variables Almost Always Required By Fnsnap
00220   let pgup=90 : let pgdn=91 : let event=98 !:
        let esc=99 : let up=102 : let left=103 !:
        let dn=104 : let home=112 !:
        let end=113
00221   let click=201 : let dblclick=202 !:
        let help=100 : let rtclick=100 !:
        let rtdblclick=100
00222   let upfld=105 : let dnfld=106 : let foflow=107 !:
        let right=109 : let left=108 : let home=112 !:
        let end=113 : let fldplus=114 : let fldminus=115
00500   return 
50000 ! DEF LIBRARY FNRTF(MAT TYPES$,MAT STYLES$,MAT CELLS$,TXTFILE,RTFFILE;LMARGIN,RMARGIN,LEFTTEXT$,NUME,LANDSCAPE)
50001   def library fnrtf(txtfile,specfile$*100,rtffile;picpath$*100,subpath$*100)
50010 ! mat TYPES$ holds single letters used to designate type!:
          ! of line such as H=header F=Footer D=Detail data T=Total line
50020 ! mat STYLES$ holds the formatting specifications followed !:
          ! by the name of the style such as HEADER !:
          ! sg,emts of the style are separated by a pipe | character !:
          ! elements of the style might be li0.5 for left indent 1/2 inch !:
          ! rt2 sets a tab at 2 inches !:
          ! dt2.5 sets a decimal tab at 2 1/2 inches
50030 ! mat DATA$ holds the body of the report !:
          ! the first character designates what style is to be applied to the !:
          ! line the remaining elements separated by pipes are the data to be !:
          ! printed using the style !:
          ! a long sentence will be wrapped by the RTF reader and so long text!:
          ! lines do not need to be broken
50040 ! RFTFILE is the file number already opened to hold the report !:
          ! this file should be a display file with an ".rtf" suffix
50050     library env$("PD")&"Core\fnsnap\RTFLIB_dll.br": fnamt$,fntext$,fntype
50051     let orientation$="PORTRAIT" !:
          let paper$="LETTER" !:
          let lmargin=rmargin=tmargin=bmargin=0.5 !:
          let nume=0
50052     mat styles$(1)=("") !:
          mat cells$(1)=("") !:
          mat types$(1)=("") !:
          let lefttext$=""
50053     let setspec=1 !:
          linput #txtfile: data$
50054     if pos(data$,"[SPEC(") then gosub SET_SPECFILE !:
            restore #txtfile: 
50056     execute "proc *"&specfile$
50057     let setspec=0
50058     if uprc$(trim$(orientation$))="LANDSCAPE" then let landscape=1 else let landscape=0
50060     let t=udim(mat types$) !:
          let crlf$=chr$(13)&chr$(10)
50070     dim s$(1)*2000,se$(1)*1000,colors$(19),data$*32000,bold$(1),fs$*100,fc$*100,sa$*100
50071     dim types$(1)*2,styles$(1)*1000,cells$(1)*1000,lefttext$*100,papersize$*100
50072     dim boxmargins$*1000,oldmargins$*1000,boxmarginsn$*1000
50073     dim subx$(1)*500
50080     let colors$(1)="[BLACK]" !:
          let colors$(2)="[BLUE]" !:
          let colors$(3)="[LTBLUE]" !:
          let colors$(4)="[LTGREEN]" !:
          let colors$(5)="[PINK]" !:
          let colors$(6)="[RED]" !:
          let colors$(7)="[YELLOW]" !:
          let colors$(8)="[WHITE]" !:
          let colors$(9)="[DKBLUE]"
50081     let colors$(10)="[BLUEGREEN]" !:
          let colors$(11)="[GREEN]" !:
          let colors$(12)="[PURPLE]" !:
          let colors$(13)="[BURGUNDY]" !:
          let colors$(14)="[LTOLIVE]" !:
          let colors$(15)="[GRAY]" !:
          let colors$(16)="[LTGRAY]" !:
          let colors$(17)="[DKGREEN]" !:
          let colors$(18)="[OLIVE]"
50082     let colors$(19)="[SEAGREEN]"
50090     mat s$(t) !:
          mat se$(t) !:
          mat lin$(t) !:
          mat bold$(t)
50100     mat lin$=("\lin0") !:
          mat bold$=("") !:
          let qa$="\ql "
50110     gosub SET_STYLES
50112     if exists(subpath$&"subtext.txt")=2 then execute "proc *"&subpath$&"subtext.txt"
50120 BUILD_RTF: linput #txtfile: data$ eof ZBUILD_RTF
50121     if pos(data$,"[SPEC(")>0 then gosub SET_SPECFILE
50122     if pos(data$,"[NEWCELL(")>0 then gosub SET_NEWCELL
50123     if pos(data$,"[FONT(")>0 then gosub SET_FONT
50124     if pos(data$,"[SUB(")>0 then gosub SET_SUB
50126     if pos(data$,"[RTFCOL(") then let xs=pos(data$,"[RTFCOL(")+8 !:
            let xe=pos(data$,")",xs)-1 !:
            let x=val(data$(xs:xe)) !:
            print #rtffile: "\sect \sectd \sbknone\linex0\cols"&str$(x)&"\sectdefaultcl " !:
            let data$=srep$(data$,"[RTFCOL("&str$(x)&")]","")
50127     if pos(data$,"[RTFCOLL(") then let xs=pos(data$,"[RTFCOLL(")+9 !:
            let xe=pos(data$,")",xs)-1 !:
            let x=val(data$(xs:xe)) !:
            print #rtffile: "\sect \sectd \sbknone\linex0\cols"&str$(x)&"\linebetcol\sectdefaultcl " !:
            let data$=srep$(data$,"[RTFCOLL("&str$(x)&")]","")
50128     if pos(data$,"[WD(") then let xs=pos(data$,"[WD(")+4 !:
            let xe=pos(data$,")]",xs)-1 !:
            let x=val(data$(xs:xe)) !:
            let data$=srep$(data$,"[WD("&str$(x)&")]",'{{\field{\*\fldinst SYMBOL '&str$(x)&' \\f "Wingdings" \\s 17}{\fldrslt\f14}}}') !:
            ! Insert WingDing symbol
50130     if trim$(data$)<="" then print #rtffile: "\par " !:
            goto BUILD_RTF
50135     let data$=srep$(data$,chr$(9),"|")
50140     if data$(1:1)="N" and pos(data$,"|")=3 then !:
            let newdata=1 !:
            let data$=data$(2:len(data$)) !:
          else !:
            let newdata=0
50142     if laststyle=1 and pos("0123456789",data$(1:1)) then print #prntfil: "\pard \ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
50150     let data$=fnrtfcolor$(data$)
50160     if data$(1:2)="D|" then 
50170       let x=len(data$)
50180       if x<30 and data$(x:x)=":" then !:
              let data$=data$(1:2)&"{\b "&data$(3:len(data$))&"}"
50190     end if 
50200 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Format Detail Step Section Code E                            ?:
50210     if uprc$(data$(1:1))="E" then 
50212       if checklist>0 then 
50213         let linecnt+=1 !:
              if pos(data$(1:7),".")>1 or pos(data$(1:7),")")>1 then !:
                let numplc=pos(data$," ") else !:
                let numplc=0
50214         if nume then !:
                let data$=data$(1:2)&"\tab "&str$(linecnt)&".)"&"\tab "&fntext$(data$(3:len(data$)),50)&"\tab "&lefttext$&"[RTFLINE]" !:
              else if numplc>0 then let data$=data$(1:numplc-1)&"\tab    "&fntext$(ltrm$(data$(numplc:len(data$))),50)&"\tab "&lefttext$&"[RTFLINE]" !:
              else let data$=fntext$(ltrm$(data$),50)&"\tab "&lefttext$&"[RTFLINE]"
50219     else 
50220       let x=0 !:
            let x=val(data$(3:7)) conv L50240
50230       if x>0 then !:
              let data$=data$(1:2)&"{\par\b "&data$(3:6)&"}"&data$(7:len(data$))
50240 L50240: if data$(3:len(data$))="Cause" then !:
              let data$(3:len(data$))="  "&data$(3:len(data$)) else !:
              let data$=srep$(data$,"Cause ","  Cause [RTFLINE]")
50250       if data$(3:len(data$))="Remedy" then !:
              let data$(3:len(data$))="  "&data$(3:len(data$)) else !:
              let data$=srep$(data$,"Remedy ","  Remedy [RTFLINE]")
50300     end if 
50310   else let linecnt=0 ! END IF
50319 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Create special processing for PROGRAM G lines                ?:
50320   if uprc$(data$(1:1))="G" then 
50330     let data$=srep$(data$,"!:","!:[RTFLINE]")
50340   end if 
50349 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Perform bracket replacement parameters                       ?:
50350   let data$=srep$(data$,"[RTFPAGE]","\page ")
50360   let data$=srep$(data$,"[RTFLINE]","\line ")
50370   let data$=srep$(data$,"[RTFDATE]",date$("Month DD, CCYY"))
50380 ! 
50390 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Draw box around text following this point                    ?:
50400   if pos(data$,"[RTFBOX]")>0 then !:
          let rtfbox=1 !:
          let data$=srep$(data$,"[RTFBOX]","") !:
          let d$=""
50410 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?End the box started by prior line                            ?:
50420   if pos(data$,"[\RTFBOX]")>0 then !:
          let rtfbox=0 !:
          let data$=srep$(data$,"[\RTFBOX]","") !:
          let d$=""
50430 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Create a line above text                                     ?:
50440   if pos(data$,"[TOPLINE]")>0 then !:
          let data$=srep$(data$,"[TOPLINE]","") !:
          let topline=1 !:
          let d$=""
50450   if pos(data$,"[\TOPLINE]")>0 then !:
          let data$=srep$(data$,"[\TOPLINE]","") !:
          let topline=0 !:
          let d$=""
50460 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Create a line below text                                     ?:
50470   if pos(data$,"[BOTLINE]")>0 then !:
          let data$=srep$(data$,"[BOTLINE]","") !:
          let botline=1 !:
          let d$=""
50480   if pos(data$,"[\BOTLINE]")>0 then !:
          let data$=srep$(data$,"[\BOTLINE]","") !:
          let botline=0 !:
          let d$=""
50490 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Create a line in the middle of a box. Verticle lines are     ?:
50500   if pos(data$,"[MIDLINE]")>0 then !:
          let midline=1 !:
          let data$=srep$(data$,"[MIDLINE]","") !:
          let d$=""
50510   if pos(data$,"[\MIDLINE]")>0 then !:
          let midline=0 !:
          let data$=srep$(data$,"[\MIDLINE]","") : let d$=""
50520 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ? Create a header to appear at the top of every page          ?:
50530   if uprc$(data$(1:1))="H" then 
50540     gosub HEADER
50550     goto L50640
50560   end if 
50570 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Create a footer to appear at the bottom of every page        ?:
50580   if uprc$(data$(1:1))="F" then 
50590     gosub FOOTER
50600     goto L50640
50610   end if 
50620   if not uprc$(data$(1:1))=d$ then gosub NEW_STYLE
50630   gosub PICTURE ! PRINT #RTFFILE: "{\par "&SREP$(DATA$(3:LEN(DATA$)),"|","}{\tab}{ ")&"}"
50640 L50640: let d$=uprc$(data$(1:1))
50650   goto BUILD_RTF
50660 ZBUILD_RTF: print #rtffile: "}"
50665   let header=0
50670 fnend 
50680 ! ==========================================================
50690 HEADER: ! 
50700 if header then 
50701   print #rtffile: "\par\sect \sectd \linex0\endnhere\sectlinegrid360\sectdefaultcl"&crlf$
50702   if lmargin>0 then print #rtffile: "\margl"&fntwips$(lmargin)&"\marglsxn"&fntwips$(lmargin)&crlf$
50703   if rmargin>0 then print #rtffile: "\margr"&fntwips$(rmargin)&"\margrsxn"&fntwips$(rmargin)&crlf$
50704   if tmargin>0 then print #rtffile: "\margt"&fntwips$(tmargin)&"\margtsxn"&fntwips$(tmargin)&crlf$
50705   if bmargin>0 then print #rtffile: "\margb"&fntwips$(bmargin)&"\margbsxn"&fntwips$(bmargin)&crlf$
50706 ! \marglsxn1800\margrsxn1440\margtsxn720\margbsxn720
50707 !  IF THEADER >0 THEN PRINT #RTFFILE: "\linex0\headery"&FNTWIPS$(THEADER)&"\footery"&FNTWIPS$(TFOOTER)&"\endnhere\sectlinegrid"&FNTWIPS$(THEADER)&"\sectdefaultcl"&CRLF$
50708 ! 
50709 end if 
50710 if botline then 
50720 ! 
50730   print #rtffile: "{\header \pard \plain "&srep$(s$(srch(mat types$,"H")),"\widctlpar","\widctlpar\brdrb\brdrs\brdrw10\brsp20 ")&"{"&srep$(srep$(data$(3:len(data$)),"|","}{\tab}{ "),"[RTFBAR]","|")&"\par }}"&crlf$ ! " {"&DATA$(3:LEN(DATA$))&" \par }}"&CRLF$
50740   let botline=0
50750 else 
50760   print #rtffile: "{\header \pard \plain "&s$(srch(mat types$,"H"))&" {"&srep$(srep$(data$(3:len(data$)),"|","}{\tab}{"),"[RTFBAR]","|")&" \par }}"&crlf$
50770 end if 
50780 let header+=1
50790 return 
50800 FOOTER: ! 
50810 if topline then 
50820   print #rtffile: "{\footer \pard \plain "&srep$(srep$(srep$(s$(srch(mat types$,"F")),"widctlpar","widctlpar\brdrt\brdrs\brdrw10\brsp20 ")&" {"&srep$(data$(3:len(data$)),"[PAGE]","  {{\field{\*\fldinst{\f4  PAGE }}{\fldrslt{\f4 2}}}}"),"|","}{\tab}{ "),"[RTFBAR]","|")&" \par }}"&crlf$
50830   let topline=0
50840 else 
50850   print #rtffile: "{\footer \pard \plain "&s$(srch(mat types$,"F"))&" {"&srep$(srep$(srep$(data$(3:len(data$)),"[PAGE]","  {{\field{\*\fldinst{\f4  PAGE }}{\fldrslt{\f4 2}}}}"),"|","}{\tab}{"),"[RTFBAR]","|")&" \par }}"&crlf$
50860 end if 
50870 let footer+=1
50880 return 
50890 NEW_STYLE: ! 
50900 ! IF SRCH(MAT TYPES$,UPRC$(DATA$(1:1)))>0 THEN PRINT #RTFFILE: "\pard \plain "&S$(SRCH(MAT TYPES$,UPRC$(DATA$(1:1))))
50910 if rtfbox then 
50911   let xlis=pos(s$(srch(mat types$,uprc$(data$(1:1)))),"\li") !:
        let xlie=pos(s$(srch(mat types$,uprc$(data$(1:1)))),"\",xlis+1)-1
50912   let boxmargins$="\li"&str$(lmargin*1440)&"\ri"&str$((lmargin+rmargin)*2*1440)
50913   let oldmargins$=s$(srch(mat types$,uprc$(data$(1:1))))(xlis:xlie)
50914   let boxmarginsn$="\aspalpha\aspnum\faauto\adjustright"&srep$(srep$(boxmargins$,"\ri","\rin"),"\li","lin")&"\itap0"
50920   if midline then 
50930     if srch(mat types$,uprc$(data$(1:1)))>0 then print #rtffile: "\pard \plain "&srep$(srep$(s$(srch(mat types$,uprc$(data$(1:1)))),"widctlpar","wictlpar\brdrt\brdrs\brdrw10\brsp20 \brdrl\brdrs\brdrw10\brsp80 \brdrb\brdrs\brdrw10\brsp20 \brdrr\brdrs\brdrw10\brsp80 \brdrbtw\brdrs\brdrw10\brsp20 "),oldmargins$,boxmargins$)&crlf$&boxmarginsn$&crlf$
50940   else 
50950     if srch(mat types$,uprc$(data$(1:1)))>0 then print #rtffile: "\pard \plain "&srep$(srep$(s$(srch(mat types$,uprc$(data$(1:1)))),"widctlpar","wictlpar\brdrt\brdrs\brdrw10\brsp20 \brdrl\brdrs\brdrw10\brsp80 \brdrb\brdrs\brdrw10\brsp20 \brdrr\brdrs\brdrw10\brsp80 "),oldmargins$,boxmargins$)&crlf$&boxmarginsn$&crlf$
50960   end if 
50970 else 
50980   if midline then 
50990     if srch(mat types$,uprc$(data$(1:1)))>0 then print #rtffile: "\pard \plain "&s$(srch(mat types$,uprc$(data$(1:1))))&crlf$
51000   else 
51010     if srch(mat types$,uprc$(data$(1:1)))>0 then print #rtffile: "\pard \plain "&s$(srch(mat types$,uprc$(data$(1:1))))&crlf$
51020   end if 
51030 end if 
51040 let d$=uprc$(data$(1:1))
51050 return 
51060 SET_STYLES: ! 
51070 let perin=1440
51080 for a=1 to t
51090   let text$=trim$(text$)
51100   let ap=0
51110 ! LET S$(A)="\s"&STR$(A+10)&"\q1 "
51120   let s$(a)="\s"&str$(a+10)&"\ql "
51130 ! ----------------------------------
51140 SET_STYLES_1: let ap+=1
51150   let tx=pos(styles$(a),"|")
51160   if tx<0 then let tx=len(styles$(a))+1
51170   let ap+=1
51180 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?First line indent                                            ?:
51190   if uprc$(styles$(a)(1:2))="FI" then 
51200     let s$(a)=s$(a)&"\fi"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51210     goto ZSET_STYLES_1
51220   end if 
51230   let ap+=1
51240 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Left indent                                                  ?:
51250   if uprc$(styles$(a)(1:2))="LI" then 
51260     let s$(a)=s$(a)&"\li"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51270     let lin$(a)="\lin"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51280     goto ZSET_STYLES_1
51290   end if 
51300 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Right indent                                                 ?:
51310   if uprc$(styles$(a)(1:2))="RI" then 
51320     let s$(a)=s$(a)&"\ri"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51330     goto ZSET_STYLES_1
51340   end if 
51350   if ap=1 then let s$(a)=s$(a)&"\li0\ri0\widctlpar" !:
        else if pos(s$(a),"widctlpar")<1 then let s$(a)=s$(a)&"\widctlpar "
51360 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Tab left align                                               ?:
51370 if uprc$(styles$(a)(1:2))="TL" then 
51380   let s$(a)=s$(a)&"\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51390   goto ZSET_STYLES_1
51400 end if 
51410 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Tab center align                                             ?:
51420 if uprc$(styles$(a)(1:2))="TC" then 
51430   let s$(a)=s$(a)&"\tqc\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51440   goto ZSET_STYLES_1
51450 end if 
51460 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Tab right align                                              ?:
51470 if uprc$(styles$(a)(1:2))="TR" then 
51480   let s$(a)=s$(a)&"\tqr\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51490   goto ZSET_STYLES_1
51500 end if 
51510 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Tab decimal point align                                      ?:
51520 if uprc$(styles$(a)(1:2))="TD" then 
51530   let s$(a)=s$(a)&"\tqdec\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51540   goto ZSET_STYLES_1
51550 end if 
51560 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Tab vertical Bar align                                       ?:
51570 if uprc$(styles$(a)(1:2))="TB" then 
51580   let s$(a)=s$(a)&"\tb"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51590   goto ZSET_STYLES_1
51600 end if 
51601 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Font if other than Times new roman (Panto....)               ?:
51604 if uprc$(styles$(a)(1:9))="FPALATINO" then let ff$="\f29" !:
        goto ZSET_STYLES_1
51605 if uprc$(styles$(a)(1:6))="FFRITZ" then let ff$="\f28" !:
        goto ZSET_STYLES_1
51606 if uprc$(styles$(a)(1:6))="FTIMES" then let ff$="\f0" !:
        goto ZSET_STYLES_1
51607 if uprc$(styles$(a)(1:6))="FARIAL" then let ff$="\f1" !:
        goto ZSET_STYLES_1
51608 if uprc$(styles$(a)(1:8))="FCOURIER" then let ff$="\f2" !:
        goto ZSET_STYLES_1
51610 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Font size                                                    ?:
51620 if uprc$(styles$(a)(1:2))="FS" then 
51630   let fs$=str$(round(val(styles$(a)(3:tx-1))*2,0))
51640   goto ZSET_STYLES_1
51650 end if 
51660 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Font color                                                   ?:
51670 if uprc$(styles$(a)(1:2))="CF" then 
51680 ! INPUT FIELDS "23,64,c 1": PAUSE$
51690   let fc$="\cf"&str$(max(0,srch(mat colors$,uprc$(styles$(a)(3:tx-1)))))&" "
51700   goto ZSET_STYLES_1
51710 end if 
51720 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Font BOLD                                                    ?:
51730 if uprc$(styles$(a)(1:1))="B" then 
51740   let bold$(a)="\b"
51750   goto ZSET_STYLES_1
51760 end if 
51770 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Space After Paragraph                                        ?:
51780 if uprc$(styles$(a)(1:2))="SA" then 
51790   let sa$="\sa"&cnvrt$("pic(####)",val(style$(a)(3:tx-1))*320)&" "
51800   goto ZSET_STYLES_1
51810 end if 
51820 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Paragraph Alignment                                          ?:
51830 if uprc$(styles$(a)(1:1))="Q" and pos("LRCJ",uprc$(styles$(a)(2:2)))>0 then 
51840   let qa$="\q"&lwrc$(styles$(a)(2:2))&" "
51850   let s$(a)=srep$(s$(a),"\ql ",qa$)
51860   goto ZSET_STYLES_1
51870 end if 
51880 ZSET_STYLES_1: let styles$(a)=styles$(a)(tx+1:len(styles$(a)))
51890 if len(styles$(a))>1 and pos(styles$(a),"|")>0 then goto SET_STYLES_1
51900 let s$(a)=s$(a)&"\aspalpha\aspnum\faauto\adjustright\rin0"&lin$(a)&"\itap0 "&bold$(a)&ff$&"\fs"&fs$&fc$&sa$&"\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 " !:
      let se$(a)="\sbasedon"&str$((a-1)+10)&" \snext"&str$(a+10)
51910 let fc$=fs$=sa$=ff$=""
51920 next a
51930 print #rtffile: "{\rtf1\ansi\ansicpg1252\uc1 \deff0\deflang1033\deflangfe1033"&crlf$
51940 print #rtffile: "{\fonttbl"&crlf$
51945 print #rtffile: "{\f0\froman\fcharset0\fprq2{\*\panose 02020603050405020304}Times New Roman;}"&crlf$
51950 print #rtffile: "{\f1\fswiss\fcharset0\fprq2{\*\panose 020b0604020202020204}Arial;}"&crlf$
51955 print #rtffile: "{\f2\fmodern\fcharset0\fprq1{\*\panose 02070309020205020404}Courier New;}"&crlf$
51956 print #rtffile: "{\f14\fnil\fcharset2\fprq2{\*\panose 05000000000000000000}Wingdings;}"&crlf$
51957 print #rtffile: "{\f28\froman\fcharset0\fprq2{\*\panose 02020500000000000000}Fritz-Quad;}"&crlf$
51958 print #rtffile: "{\f29\froman\fcharset0\fprq2{\*\panose 02040502050505030304}Palatino Linotype;}"&crlf$
51960 print #rtffile: "{\f69\fmodern\fcharset0\fprq1{\*\panose 020b0609040504020204}Lucida Console;}"&crlf$
51962 ! 
51970 print #rtffile: "}"&crlf$
51980 print #rtffile: "{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;"&crlf$
51990 print #rtffile: "\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;\red128\green128\blue128;\red192\green192\blue192;\red255\green255\blue255;\red217\green217\blue217;}"&crlf$
52000 print #rtffile: "{\stylesheet"&crlf$
52010 print #rtffile: "{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \snext0 Normal;}"&crlf$
52020 print #rtffile: "{\*\cs10 \additive Default Paragraph Font;}"&crlf$
52030 for a=1 to t
52040   print #rtffile: "{"&s$(a)&se$(a)&" "&styles$(a)&";}"&crlf$
52050 next a
52060 print #rtffile: "}"&crlf$
52070 print #rtffile: "{\info"&crlf$
52080 print #rtffile: "{\title This is the first line}"&crlf$
52090 print #rtffile: "{\title This is the first line}"&crlf$
52100 print #rtffile: "{\operator George L. Tisdale}"&crlf$
52110 print #rtffile: "{\creatim\yr"&date$("ccyy")&"\mo"&date$("mm")&"\dy"&date$("dd")&"\hr"&time$(1:2)&"\min"&time$(4:5)&"}"&crlf$
52120 print #rtffile: "{\revtim\yr"&date$("ccyy")&"\mo"&date$("mm")&"\dy"&date$("dd")&"\hr"&time$(1:2)&"\min"&time$(4:5)&"}"&crlf$
52130 print #rtffile: "{\version2}"&crlf$
52140 print #rtffile: "{\edmins0}"&crlf$
52150 print #rtffile: "{\nofpages1}"&crlf$
52160 print #rtffile: "{\nofwords0}"&crlf$
52170 print #rtffile: "{\nofchars0}"&crlf$
52180 print #rtffile: "{\*\company Advanced Informonics Corporation}"&crlf$
52190 print #rtffile: "{\nofcharsws0}"&crlf$
52200 print #rtffile: "{\vern8247}"&crlf$
52210 if lmargin>0 then print #rtffile: "\margl"&str$(ip(lmargin*1440))&crlf$
52220 if rmargin>0 then print #rtffile: "\margr"&str$(ip(rmargin*1440))&crlf$
52222 if tmargin>0 then print #rtffile: "\margt"&str$(ip(tmargin*1440))&crlf$
52224 if bmargin>0 then print #rtffile: "\margb"&str$(ip(bmargin*1440))&crlf$
52230 print #rtffile: "}"&crlf$ !:
      let papersize$="\paperw12240\paperh15840"
52231 if uprc$(trim$(paper$))="LETTER" or paper$<="" then 
52232   if uprc$(trim$(orientation$))="LANDSCAPE" then !:
          let papersize$="\paperw15840\paperh12240" else !:
          let papersize$="\paperw12240\paperh15840"
52233 end if 
52234 ! 
52235 if uprc$(trim$(paper$))="LEGAL" then 
52236   if uprc$(trim$(orientation$))="LANDSCAPE" then !:
          let papersize$="\paperw20160\paperh12240" else !:
          let papersize$="\paperw12240\paperh20160"
52237 end if 
52238 print #rtffile: papersize$&crlf$
52239 ! IF UPRC$(TRIM$(ORIENTATION$))="LANDSCAPE" THEN PRINT #RTFFILE: PAPERSIZE$&CRLF$
52240 print #rtffile: "\widowctrl\ftnbj\aenddoc\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\hyphcaps0\formshade\horzdoc\dgmargin\dghspace180\dgvspace180\dghorigin720\dgvorigin720\dghshow1\dgvshow1"&crlf$
52245 dim theader$*100
52246 if theader>0 and tfooter>0 then !:
        let theader$="\headery"&fntwips$(theader)&"\footery"&fntwips$(tfooter) else if theader>0 then !:
        let theader$="\headery"&fntwips$(theader) else if tfooter>0 then let theader$="\footery"&fntwips$(tfooter) else !:
        let theader$=""
52250 print #rtffile: "\jexpand\viewkind4\viewscale100\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule \fet0\sectd \linex0"&theader$&"\endnhere\sectlinegrid360\sectdefaultcl "&crlf$
52255 ! IF LANDSCAPE=1 THEN PRINT #RTFFILE: "\lndscpsxn\psz1"
52256 if landscape=1 then print #rtffile: "\landscape"
52260 return 
52270 def library fnamt$*50(value,mask$*20;sign$,underline$,reverse)
52280   dim av$*30
52290   if reverse>0 then let reverse=-1 else let reverse=1
52300   let underline$=uprc$(underline$)
52310   on pos("SD",underline$) goto SINGLE,DOUBLE none NO_UNDERLINE
52320 NO_UNDERLINE: ! 
52330   if value*reverse<0 then let av$=sign$&"(\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{)" else let av$=sign$&" \tab "&trim$(cnvrt$(mask$,abs(value)))&"}{ "
52340   goto ZAMT
52350 SINGLE: ! 
52360   if value*reverse<0 then let av$=sign$&"(\ul\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{)" else let av$=sign$&" \ul\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{ "
52370   goto ZAMT
52380 DOUBLE: ! 
52390   if value*reverse<0 then let av$=sign$&"(\uldb\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{)" else let av$=sign$&"\uldb\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{ "
52400   goto ZAMT
52410 ZAMT: let fnamt$=av$
52420 fnend 
52430 def library fntext$*4000(text$*4000,alen)
52440   dim t$*4000
52450   let t$=""
52460 L52460: let text$=trim$(text$)
52461   print text$
52462   if text$(1:9)="[RTFLINE]" then let text$(1:9)=""
52463   let xl=min(pos(text$,"[RTFLINE]",9)-1,alen) !:
        if xl<1 then let xl=alen else !:
          if xl>alen then let xl=alen
52464   print "XL="&str$(xl),"ALEN="&str$(alen)
52465 ! PAUSE
52470   let x=pos(text$&" "," ",xl)
52480   if x<2 then goto L52520
52490   if len(t$) then let t$=t$&"\line "&text$(1:x) else let t$=text$(1:x)
52500   let text$=trim$(text$(x:len(text$)))
52510   if len(trim$(text$))>xl then goto L52460
52520 L52520: ! IF LEN(TEXT$) THEN LET T$=T$&"\line "&TEXT$
52530   if len(t$)>0 and len(text$) then let t$=t$&"\line "&text$ else if len(text$)>0 then let t$=t$&text$
52540 let fntext$=t$
52545 print t$
52546 print "*********************************************"
52547 ! PAUSE
52550 fnend 
52560 def fnrtfcolor$*6000(cd$*6000)
52561   let acolor=0
52562   do while acolor<19
52563     let acolor+=1
52564     if pos(cd$,colors$(acolor))>0 then let cd$=srep$(cd$,colors$(acolor),"\cf"&str$(acolor)&" ")
52565   loop 
52568   let fnrtfcolor$=cd$
52640 fnend 
52700 ! --------------------------------
52710 SET_NEWCELL: ! 
52720 let ncs=pos(data$,"[NEWCELL(")+9
52730 let nce=pos(data$,")]",ncs)-1
52735 ! PAUSE
52740 if exists(picpath$&trim$(data$(ncs:nce))) then execute "proc *"&picpath$&trim$(data$(ncs:nce)) else input fields "10,10,c 10": pause$
52750 let data$=srep$(data$,"[NEWCELL("&data$(ncs:nce)&")]","")
52755 ! PAUSE
52760 return 
52770 ! ---------------------------------
52780 SET_FONT: ! 
52782 let nfs=pos(data$,"[FONT(")+6 !:
      let nfe=pos(data$,")]",nfs)-1
52784 if nfs>0 then !:
        let data$=srep$(data$,"[FONT(ARIAL)]","\f1 ") !:
        let data$=srep$(data$,"[FONT(COURIER)]","\f2 ") !:
        let data$=srep$(data$,"[FONT(FRITZ)]","\f28 ") !:
        let data$=srep$(data$,"[FONT(PALATINO)]","\f29 ") !:
        let data$=srep$(data$,"[FONT(LUCIDA)]","\f69 ")
52786 return 
52800 SET_SUB: ! substitute values
52802 let nss=pos(data$,"[SUB(")+5 !:
      let nse=pos(data$,")]",nss)-1
52804 if nse>0 then 
52806   execute "let subx="&data$(nss:nse)
52808   if subx<1 then !:
          let msgbox("The replaceable parameter "&data$(nss-1:nse+1)&" was not found in the data-set. The parameter will be ommitted") !:
          let data$(nss-5:nse+2)="" !:
        else let subx$=data$(nss-5:nse+2)
52810   if subx>0 then let data$=srep$(data$,subx$,sub$(subx))
52812   goto SET_SUB
52814 end if 
52816 return 
53000 PICTURE: !:
      ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Merge in RTF picture file and print final DATA line          ?:
53005 dim pict_name$*100
53007 if newdata then print #rtffile: "{\cf9 " !:
        ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Sets text of newlines to BLUE                                ?:
53010 if pos(data$,"[PICT(") >0 then 
53020   let pict_start=pos(data$,"[PICT(")
53030   let pict_end=pos(data$,")]",pict_start)
53040   print #rtffile: "{\par \qc "&srep$(data$(3:pict_start-1),"|","}{\tab}{")
53050   if picpath$>"" then !:
          let pict_name$=picpath$&"\"&trim$(data$(pict_start+6:pict_end-1)) else !:
          let pict_name$=trim$(data$(pict_start+6:pict_end-1))
53060   let fntype(pict_name$,rtffile)
53065   let x=pos(pict_name$,"\",-1)+1 !:
        print #rtffile: "{\par \fs16 Figure: "&pict_name$(x:len(pict_name$))&" \par }"
53070   print #rtffile: srep$(data$(pict_end+2:len(data$)),"|","}{\tab}{")&"}"&crlf$
53080 else if pos("1234567890",data$(1:1))>0 then 
53082   let fncells(rtffile,data$,mat cells$)
53085 else 
53090   print #rtffile: "{"&srep$(srep$(data$(3:len(data$)),"|","}{\tab}{"),"[RTFBAR]","|")&"\par }"&crlf$
53100 end if 
53103 if newdata then print #rtffile: "}" !:
        ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ? Turns off BLUE newline text                                 ?:
53105 let laststyle=0
53110 return 
53999 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
      ! ?Create table lines and print to existing print file          ?:
54000 def fncells(prntfil,cdata$*1000,mat ccells$)
54010 ! prntfil is the already open rtfoutput file !:
        ! cdata$ is the copy of DATA$ being used by the function !:
        ! mat ccells$ is the matrix of cell definitions received as MAT cells$
54020 ! INPUT FIELDS "24,64,c 1": PAUSE$
54030   dim brdr$*500,celldata$(1)*1000,call$*500,csize$(1)*500,celldef$*3000
54040   let css=cse=0 ! starting position
54050 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Parse the incoming data line to determine what table         ?:
54060 SETCSS: let css=cse+1
54070   let cse=min(len(cdata$)+1,pos(cdata$&"|","|",css))
54080   if css=1 then 
54090     let cell=val(cdata$(css:cse-1))
54100     let cnum=0
54110     goto SETCSS
54120   end if 
54130 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ? Set matrix CELLDATA$ to contents of mat cells$              ?:
54140   if css<len(cdata$) then 
54150     let cnum+=1 !:
          mat celldata$(cnum) !:
          let celldata$(cnum)=cdata$(css:cse-1)
54160     goto SETCSS
54170   end if 
54175 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Begin building table definition parameters from CELLS$(cell) ?:
54180   let cnum=css=cse=0 !:
        let cface$=cfont$="" !:
        let call$=crlf$&"\trowd \trgaph108\trleft-108"
54190 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ? Build the RTF properties for the table line needed          ?:
54200 SETCELL: let css=cse+1
54210   let cse=pos(ccells$(cell)&"|","|",css)
54220   if cse<=len(ccells$(cell)) then 
54230 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Left side indent if any                                      ?:
54240     if pos(ccells$(cell)(css:cse-1),"li")=1 then !:
            let cindent$=fntwips$(val(ccells$(cell)(css+2:cse-1))) !:
            let cindent=val(cindent$) !:
            let call$=call$&"\trleft"&cindent$ !:
            goto SETCELL
54250 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Set the distance between cells.  The number is 1/2 the       ?:
54260     rem IF POS(CCELLS$(CELL)(CSS:CSE-1),"tg")=1 THEN !:                                           LET X$=FNTWIPS$(.5*VAL(CCELLS$(CELL)(CSS+2:CSE-1))) !:                                    LET CALL$=SREP$(CALL$,"\trgaph108\trleft-108","\trgaph"&X$&"\trleft-"&X$&" ") !:          GOTO SETCELL
54261     if pos(ccells$(cell)(css:cse-1),"tg")=1 then !:
            let x$=fntwips$(.5*val(ccells$(cell)(css+2:cse-1))) !:
            let call$=srep$(call$,"\trgaph108\trleft-108","\trgaph"&x$) !:
            goto SETCELL
54269 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Designate header rows for repeat on subsequent pages         ?:
54270     if pos(ccells$(cell)(css:cse-1),"trh")=1 then !:
            let call$=call$&"\trhdr" !:
            goto SETCELL
54272 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Designate row height in postive inches for AT LEAST height   ?:
54274     if pos(ccells$(cell)(css:cse-1),"trrh")=1 then !:
            let call$=call$&"\trrh"&fntwips$(val(ccells$(cell)(css+4:cse-1))) !:
            goto SETCELL
54280 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Set font size in points                                      ?:
54290     if pos(ccells$(cell)(css:cse-1),"fs")=1 and cnum=0 then !:
            let cfont$="\fs"&str$(val(ccells$(cell)(css+2:cse-1))*2)&"" !:
            goto SETCELL
54291 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Set font type face                                           ?:
54292     if uprc$(ccells$(cell)(css:cse-1))="FTIMES" and cnum=0 then !:
            let cface$="\f0" !:
            goto SETCELL
54293     if uprc$(ccells$(cell)(css:cse-1))="FARIAL" and cnum=0 then !:
            let cface$="\f1" !:
            goto SETCELL
54294     if uprc$(ccells$(cell)(css:cse-1))="FCOURIER" and cnum=0 then !:
            let cface$="\f2" !:
            goto SETCELL
54295     if uprc$(ccells$(cell)(css:cse-1))="FFRITZ" and cnum=0 then !:
            let cface$="\f28" !:
            goto SETCELL
54297     if uprc$(ccells$(cell)(css:cse-1))="FPALATINO" and cnum=0 then !:
            let cface$="\f29" !:
            goto SETCELL
54298     if uprc$(ccells$(cell)(css:cse-1))="FLUCIDA" and cnum=0 then !:
            let cface$="\f69" !:
            goto SETCELL
54300 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Build formats for each Cell left to right                    ?:
54310     if pos(ccells$(cell)(css:cse-1),"c")=1 then 
54320       let cnum+=1 !:
            mat cfmt$(cnum): mat csize$(cnum) : mat csize(cnum) !:
            let cfmt$(cnum)="" : mat cface$(cnum) : mat cfont$(cnum) !:
            let cface$(cnum)=cface$ !:
            let cfont$(cnum)=cfont$
54330       let csize(cnum)=val(fntwips$(val(ccells$(cell)(css+1:cse-1))))
54340       rem LET CSIZE$(CNUM)="\cellx"&STR$(SUM(CSIZE)+CINDENT) !:                                     GOTO SETCELL
54341       let csize$(cnum)="\cellx"&str$(sum(csize)+cindent) !:
            goto SETCELL
54350     end if 
54360 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Determine border width from 0 to 5 (0 to 75 TWIPS)           ?:
54370     if pos(ccells$(cell)(css:cse-1),"br")=1 then 
54380       let brdr$=""
54390       if pos(cells$(cell)(css+2:cse),"s")>0 then let brdrtyp$="s" else !:
              if pos(cells$(cell)(css+2:cse),"o")>0 then let brdrtyp$="o" else !:
                if pos(cells$(cell)(css+2:cse),"d")>0 then let brdrtyp$="d" else !:
                  if pos(cells$(cell)(css+2:cse),"a")>0 then let brdrtyp$="a" else !:
                    let brdrtyp$="s"
54400       let brdrw=min(5,val(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(ccells$(cell)(css:cse-1),"b",""),"t",""),"r",""),"l",""),"b",""),"a",""),"s",""),"d",""),"o","")))*15
54410       if (x:=pos(ccells$(cell)(css+2:cse-1),"t"))>0 then !:
              let brdr$="\clbrdrt"&fnbrdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
54420       if (x:=pos(ccells$(cell)(css+2:cse-1),"r"))>0 then !:
              let brdr$=brdr$&"\clbrdrr"&fnbrdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
54430       if (x:=pos(ccells$(cell)(css+1:cse-1),"l"))>0 then !:
              let brdr$=brdr$&"\clbrdrl"&fnbrdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
54440       if (x:=pos(ccells$(cell)(css+1:cse-1),"b"))>0 then !:
              let brdr$=brdr$&"\clbrdrb"&fnbrdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
54450       let csize$(cnum)=brdr$&csize$(cnum)
54460       goto SETCELL
54470     end if 
54480 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Set vertical alignment for each cell                         ?:
54490     if pos(ccells$(cell)(css:cse-1),"v")=1 then 
54500       if pos(ccells$(cell)(css+1:cse-1),"t")=1 then let csize$(cnum)=srep$(csize$(cnum),"\cellx","\clvertalt\cellx")
54510       if pos(ccells$(cell)(css+1:cse-1),"c")=1 then let csize$(cnum)=srep$(csize$(cnum),"\cellx","\clvertalc\cellx")
54520       if pos(ccells$(cell)(css+1:cse-1),"b")=1 then let csize$(cnum)=srep$(csize$(cnum),"\cellx","\clvertalb\cellx")
54530       goto SETCELL
54540     end if 
54550 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Set horizontal alignment of each cell                        ?:
54560     if pos(ccells$(cell)(css:cse-1),"h")=1 then 
54570       if pos(ccells$(cell)(css+1:cse-1),"r")=1 then let cfmt$(cnum)=cfmt$(cnum)&"\qr"
54580       if pos(ccells$(cell)(css+1:cse-1),"c")=1 then let cfmt$(cnum)=cfmt$(cnum)&"\qc"
54590       if pos(ccells$(cell)(css+1:cse-1),"l")=1 then let cfmt$(cnum)=cfmt$(cnum)&"\ql"
54600       goto SETCELL
54610     end if 
54620 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Set the cell font if different than the default              ?:
54630     if uprc$(ccells$(cell)(css:cse-1))="FTIMES" then !:
            let cface$(cnum)="\f0 " !:
            goto SETCELL
54640     if uprc$(ccells$(cell)(css:cse-1))="FARIAL" then !:
            let cface$(cnum)="\f1 " !:
            goto SETCELL
54650     if uprc$(ccells$(cell)(css:cse-1))="FCOURIER" then !:
            let cface$(cnum)="\f2 " !:
            goto SETCELL
54660     if uprc$(ccells$(cell)(css:cse-1))="FFRITZ" then !:
            let cface$(cnum)="\f28 " !:
            goto SETCELL
54661     if uprc$(ccells$(cell)(css:cse-1))="FPALATINO" then !:
            let cface$(cnum)="\f29 " !:
            goto SETCELL
54662     if uprc$(ccells$(cell)(css:cse-1))="FLUCIDA" then !:
            let cface$(cnum)="\f69 " !:
            goto SETCELL
54670 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Set the cell font if different than the default              ?:
54680     if pos(ccells$(cell)(css:cse-1),"fs")=1 and cnum>0 then !:
            let cfont$(cnum)="\fs"&str$(val(ccells$(cell)(css+2:cse-1))*2)&" " !:
            goto SETCELL
54690 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
          ! ?Set the backround shading of each cell                       ?:
54700     if pos(ccells$(cell)(css:cse-1),"sh")=1 then 
54710       let csize$(cnum)=srep$(csize$(cnum),"\cellx","\clshdng"&str$(val(ccells$(cell)(css+2:cse-1))*100)&" "&"\cellx")
54720       goto SETCELL
54730     end if 
54740     goto SETCELL
54750   end if 
54760 PRINT_CELLS: !:
        ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Start the printing of the CELL row                           ?:
54770   mat celldata$(cnum) !:
        ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Make the data matrix the same size as the number of cells    ?:
54780 ! INPUT FIELDS "24,64,c 1": PAUSE$
54790   let celldef$=call$
54800 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Define the cell row and number and size of cells             ?:
54810   for a=1 to cnum
54820     if a=cnum then let celldef$=celldef$&csize$(a)&crlf$ else let celldef$=celldef$&csize$(a)
54830   next a
54840 ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Print the cells                                              ?:
54850   print #prntfil: cface$&cfont$&celldef$
54860   for a=1 to cnum
54870     if a=1 then print #prntfil: "\pard \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
54880     print #prntfil: cfmt$(a)&"{"&cface$(a)&cfont$(a)&srep$(srep$(celldata$(a),"|","}{ \tab }{ "),"[RTFBAR]","|")&"\cell }"&crlf$
54890     if a=cnum then print #prntfil: "\pard \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
54900   next a
54910   print #prntfil: "{"&celldef$&crlf$&"\row }"&crlf$
54920   print #prntfil: "\pard \ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
54930   let laststyle=1
54990 fnend 
55000 def fnbrdr$(btype$)
55010   let fnbrdr$=""
55020   if btype$="s" then let fnbrdr$="\brdrs"
55030   if btype$="o" then let fnbrdr$="\brdrdot"
55040   if btype$="a" then let fnbrdr$="\brdrdash"
55050   if btype$="d" then let fnbrdr$="\brdrdb"
55060 fnend 
55100 def fntwips$(inch)
55110   if inch>0 then let twips$=str$(round(inch*1440,0))
55112   if inch<0 then let twips$="-"&str$(round(abs(inch)*1440,0))
55114   if inch=0 then let twips$=""
55120   let fntwips$=twips$
55130 fnend 
55200 ! --------------------------------
55210 SET_SPECFILE: ! 
55220 let sps=pos(data$,"[SPEC(")+6
55230 let spe=pos(data$,")]",sps)-1
55240 if not setspec then goto L55260
55250 if exists(picpath$&trim$(data$(sps:spe))) then let specfile$=picpath$&trim$(data$(sps:spe)) else let pause : let msgbox("Designated SPEC file does not exist.") : input fields "10,10,c 10": pause$
55260 L55260: let data$=srep$(data$,"[SPEC("&data$(sps:spe)&")]","")
55270 ! PAUSE
55280 return 
60000 def library fntype(infile$*100,outfile) !:
        ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Moves the contents of one file into another without using      ?:
60010 ! INPUT FIELDS "23,60, C1": PAUSE$
60020   let infile=30
60030 L60030: if file(infile)<0 then goto L60050
60040   let infile+=1 !:
        goto L60030
60050 L60050: open #infile: "name="&infile$&",recl=1",external,input 
60060   let infile_lrec=lrec(infile)
60070   close #infile: 
60080   let infile_recl=min(32000,infile_lrec)
60090   open #infile: "name="&infile$&",RECL="&str$(infile_recl),external,input,relative 
60100   let infile_rec=0
60110   let infile_frm$="FORM C "&str$(infile_recl)
60120   if infile_recl=32000 then 
60130 L60130: if infile_rec*infile_recl+infile_recl<=infile_lrec then 
60140       read #infile,using infile_frm$: inrec$ !:
            print #outfile: inrec$ !:
            let infile_rec+=1
60150       goto L60130
60160     else 
60170       let infile_frm$="FORM C "&str$(infile_lrec-infile_rec*32000)
60180       close #infile: 
60190       open #infile: "name="&infile$&",RECL="&str$(infile_lrec-infile_rec*32000),external,input,relative 
60200     end if 
60210   end if 
60220   read #infile,using infile_frm$,pos=infile_rec*infile_recl+1: inrec$ !:
        print #outfile: inrec$
60230   dim inrec$*32000
60240 ZTYPE: close #infile: !:
        let infile=0
60250 fnend 
