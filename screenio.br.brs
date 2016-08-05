00010 !  #Autonumber# 10,10
00020 !  Screenio - Copyright June 2008 By Gabriel Bakker - All Rights Reserved
00030 !  This Library Is A Combination Screen Generator / All Purpose Fm
00040 !  Screenio Builds Upon Fileio And Is Dependent Upon Fileio
00050 ! 
00060 !  If You Run This Library As A Program, It Will Function As A Screen
00070 !  Generator That Generates And Maintains The Sceen Data Used To Design
00080 !  And Store Your Custom File Maintenance Routines.
00090 !  HI MOM IT"S ME
00100 !  If You Link To This Library And Call It As A Function Library,
00110 !  Calling Function Fnfm("scrnname") (or fnfm$) Will Run The File
00120 !  Maintenance Routine Titled Scrnname And Modify The Datafile That
00130 !  Routine Is Designed To Modify.
00140 ! 
00150 !  Special Thanks to all our Supporters (Ctrl \/ to find details)
00160 ! 
00170 ! 
02100 !  #Autonumber# 2100,10
02110   let fndesignscreen
02120   execute "system"
02130 ! 
02140   print "This is the screenio runtime library and designer. It is used"
02150   print "creating and running screenIO programs."
02160   print 
02170   print "If want to know more information about ScreenIO Business"
02180   print "Rules Rapid Development Technologies, contact Gabriel Bakker"
02190   print "of Sage AX, at gabriel.bakker@gmail.com."
02200   end 
02210 ! 
03000 !  #Autonumber# 3000,10
03010 !  ***************************************************************
03020 !  *                       Default Settings                      *
03030 !  ***************************************************************
03040 !  To modify these settings, make your own ScreenIO.ini file
03050 !  and place it in the root folder, or the screenio folder.
03060 !  copy and past the following default settings into it and
03070 !  modify them as necessary.
03080 ! 
03090 !  You want only the actual settings, not the function definition.
03100 ! 
03110   def fndefaultsettings
03120     let setting_enablelogging=0
03130     let setting_fileiopath$="fileio"
03140     let setting_screeniopath$="screenio"
03150     let setting_clockpath$="clocks\clock"
03160     let setting_imagepath$="images"
03170     let setting_brexecutable$="auto"
03180     let setting_batchfilecommand$="none"
03190     let setting_screenfolder$="screenio"
03200     let setting_functionfolder$="function\"
03210     let setting_clicktomove=1
03220     let setting_previewlistviews=1
03230     let setting_realtimefilters=0
03240     let setting_detailedcomments=1
03250   fnend 
03260 ! 
03270   def fnparsesettings
03280     if setting_functionfolder$(len(setting_functionfolder$):len(setting_functionfolder$))><"\" then let setting_functionfolder$=setting_functionfolder$&"\"
03290     if setting_screenfolder$(len(setting_screenfolder$):len(setting_screenfolder$))="\" then let setting_screenfolder$=setting_screenfolder$(1:len(setting_screenfolder$)-1)
03300     if setting_imagepath$(len(setting_imagepath$):len(setting_imagepath$))="\" then let setting_imagepath$=setting_imagepath$(1:len(setting_imagepath$)-1)
03310     if setting_clockpath$(len(setting_clockpath$):len(setting_clockpath$))="\" then let setting_clockpath$=setting_clockpath$(1:len(setting_clockpath$)-1)
03320   fnend 
03330 ! 
04000 !  #Autonumber# 4000,10
04010 !  ***************************************************************
04020 !  *                       Screen Designer                       *
04030 !  ***************************************************************
04040 SCREENDESIGNERDIMS: ! Dimension Variables
04050 ! 
04060   dim setting_enablelogging
04070   dim setting_previewlistviews
04080   dim setting_realtimefilters
04090   dim setting_detailedcomments
04100   dim setting_fileiopath$*255
04110   dim setting_screeniopath$*255
04120   dim setting_clockpath$*255
04130   dim setting_batchfilecommand$*1024
04140   dim setting_brexecutable$*255
04150   dim setting_imagepath$*255
04160   dim setting_screenfolder$*255
04170   dim setting_functionfolder$*255
04180 ! 
04190 !  File Arrays
04200   dim screenio$(1)*255, screenio(1)
04210 ! 
04220 !  Screen Control Arrays
04230   dim controlname$(1)*50, fieldname$(1)*50, description$(1)*255
04240   dim vposition(1), hposition(1),fieldtype$(1)*8, specwidth(1), width(1), height(1)
04250   dim truevalue$(1)*60, falsevalue$(1)*60
04260   dim function$(1)*255, picture$(1)*255, parent$(1)*20
04270   dim fgcolor$(1)*6, bgcolor$(1)*6, justify$(1)*1, attr$(1)*128, multiselect(1), gridlines(1)
04280   dim protected(1), invisible(1), tooltip$(1)*255, cnvrtin$(1)*255, cnvrtout$(1)*255
04290   dim userdata$(1)*255
04300 ! 
04310   dim form$(1)*2000
04320   dim fieldsssubs$(1)*50
04330   dim fieldsnsubs$(1)*50
04340 ! 
04350   dim framewindows(1)
04360   dim framekeys$(1)
04370   dim framespeccontrol$(1)*255
04380 ENDSCREENDESIGNERDIMS: ! 
04390 ! 
04400   dim fieldssspec$(1)*30
04410   dim fieldsnspec$(1)*30
04420   dim fieldssdescription$(1)*255
04430   dim fieldsndescription$(1)*255
04440 ! 
04450   dim selectedcontrols(1)
04460 ! 
11800 !  #Autonumber# 11800,10
11810 CONSTANTS: ! Define Preprocessed Constants
11820 !   .   #Define#  [[Screencontrols]]   = "mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$, mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat Multiselect, mat UserData$, mat GridLines"
11830 !   .   #Define#  [[Screencontrols1]]  = "mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$,"
11840 !   .   #Define#  [[Screencontrols2]]  = " mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat MultiSelect,mat UserData$,mat GridLines"
11850 ! 
12000 !  #Autonumber# 12000,10
12010 DESIGNSCREEN: !  Preforms Environment Testing And Launches The Screen Designer
12020   def fndesignscreen(;screenname$)
12030     if trim$(env$("guimode"))="" then 
12040       print "Please use a New GUI version of BR to design your screens."
12050     else 
12060       let fnloadandeditscreen(screenname$)
12070     end if 
12080   fnend 
12090 ! 
13000 !  #Autonumber# 13000,10
13010 LOADANDEDITSCREEN: !  Load Files And Launch Main Screen Designer
13020   def fnloadandeditscreen(;screenname$,___,window,turnguibackoff, fscreenio,fscreenfld,saveonexit,oldrows,oldcols,forcevisibility)
13030 ! 
13040     let fnsettings ! Also includes library linkage to FileIO
13050 ! 
13060     let fnopenscreenfiles(mat screenio$,mat screenio)
13070 ! 
13080     if fnreadscreen(screenname$, mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then !  Successful Read
13090       let turnguibackoff=(env$("guimode")=="OFF")
13100       if turnguibackoff then execute "config gui on"
13110 ! 
13120       let fnsetforcevisibility(0)
13130       let fnreadscreensize(oldrows,oldcols)
13140 ! 
13150       let fneditscreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
13160 ! 
13170       open #0: "rows="&str$(oldrows)&",cols="&str$(oldcols),display,outin 
13180       if turnguibackoff then execute "config gui off"
13190       let fnresetforcevisibility
13200     end if 
13210   fnend 
13220 ! 
14000 !  #Autonumber# 14000,10
14010 EDITSCREEN: ! Main Screen Designer
14020   def fneditscreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,wdebug,weditor,wtoolbar,debugsize,wactivewindow,mode,control,function)
14030     dim displayref_screenio$(1)*255
14040     dim displayref_screenio(1)
14050 ! 
14060     mat displayref_screenio$(udim(mat screenio$))
14070     mat displayref_screenio(udim(mat screenio))
14080 ! 
14090     let fndefineinputmodes
14100     do 
14110 ! 
14120 ! .   !  Update Screen If Anything Changes
14130       if ~(fnsameas(mat displayref_screenio$,mat screenio$) and fnsamea(mat displayref_screenio,mat screenio)) then 
14140         let fnredrawentirescreen(wtoolbar,wdebug,weditor,mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
14150 ! 
14160         mat displayref_screenio$=screenio$
14170         mat displayref_screenio=screenio
14180 ! 
14190         let fnrepopulatefieldslistview(screenio$(si_filelay))
14200         let displayref_screenio$(si_filelay)=screenio$(si_filelay)
14210       else 
14220 ! .      !  Update Field List Dropdown If It Changes
14230         if displayref_screenio$(si_filelay)<>screenio$(si_filelay) then 
14240           let fnrepopulatefieldslistview(screenio$(si_filelay))
14250           let displayref_screenio$(si_filelay)=screenio$(si_filelay)
14260         end if 
14270       end if 
14280       let fncolordebugactive((mode==inputdebugmode))
14290       let fncolorfieldsactive((mode==inputfieldlistmode))
14300       let fncolorattributeactive((mode==inputattributesmode),mat screenio$,mat screenio)
14310 ! 
14320       if fnvalidatefields(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then !  Update Validate Drop Down
14330 ! .         ! if we're in Movement mode or fields list mode, dont erase dots
14340         if (mode=inputeditormovemode or mode=inputeditormode or mode=inputlistviewmode) and (lastmode=inputeditormovemode or lastmode=inputeditormode or lastmode=inputlistviewmode) then 
14350           let fnrenderscreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
14360         else 
14370           let fnrenderscreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
14380         end if 
14390       end if 
14400 ! 
14410       let lastmode=mode
14420       let fncheckpoint(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) ! Save Undo Information
14430 ! 
14440       let function=fnpreforminput(mode,control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
14450       let fnprocesscommand(function, curfld, mode,control,mat screenio$,mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
14460 ! 
14470     loop until mode=quitmode
14480 ! 
14490     let fntrytoclose(weditor)
14500     let fnclosedebugwindow
14510     let fnclosetoolbarwindow
14520     let fncloseeditorwindow
14530 ! 
14540     let fnclearwindowsmenu
14550     let fnclosescreenfiles
14560 ! 
14570     close #0: 
14580 ! 
14590   fnend 
14600 ! 
16000 !  #Autonumber# 16000,5
16005 ! *****************************************************************
16010 ! ************************ Main Input Loop ************************
16015 ! *****************************************************************
16020 DEFINEMODES: !  Define The Input Spec Modes
16025   def fndefineinputmodes
16030     dim inputattributesmode
16035     dim inputfieldlistmode
16040     dim inputeditormode
16045     dim inputeditormovemode
16050     dim inputdebugmode
16055     dim quitmode
16060     dim selectcolormode
16065     dim selectfilelaymode
16070     dim inputlistviewmode
16075     dim selecteventsmode
16080     dim settabordermode
16085     dim configuredebugmode
16090     dim configureprotectedmode
16095 ! 
16100     let inputattributesmode=1
16105     let inputfieldlistmode=2
16110     let inputeditormode=3
16115     let inputeditormovemode=4
16120     let inputdebugmode=5
16125     let quitmode=6
16130     let selectcolormode=7
16135     let selectfilelaymode=8
16140     let inputlistviewmode=9
16145     let selecteventsmode=10
16150     let settabordermode=11
16155     let configuredebugmode=12
16160     let configureprotectedmode=13
16165   fnend 
16170 ! 
16175 PREFORMINPUT: !  Preform Main Input Operation
16180   def fnpreforminput(&mode,&control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,window,function,tempcolor$,index)
16185     dim inputtooltips$(1)*255
16190     dim inputspec$(1)*60
16195     dim inputdata$(1)*255
16200     dim inputttip$(1)*255
16205     dim inputsubs(1)
16210     dim inputspec$*40
16215     dim inputdata
16220     dim inputonlyspec$(1)*60
16225     dim inputonlydata$(1)*255
16230     dim inputonlyttip$(1)*255
16235 ! 
16240 ! .   ! Ensure that Mode is Valid
16245     if mode<1 or mode>13 then let mode=inputfieldlistmode
16250     if mode=inputfieldlistmode and fngetfieldscount=0 then let mode=inputattributesmode
16255     if (mode=inputlistviewmode or mode=inputeditormode or mode=inputeditormovemode or mode=settabordermode) and (udim(fieldtype$)=0) then let mode=inputattributesmode
16260     if mode=inputeditormode and udim(mat fieldtype$)>=control and lwrc$(fieldtype$(control))="listchld" then 
16265       let mode=inputlistviewmode
16270       for index=1 to udim(mat parent$)
16275         if lwrc$(trim$(parent$(index)))=lwrc$(trim$(parent$(control))) and lwrc$(fieldtype$(index))="listview" then 
16280           let control=index
16285         end if 
16290       next index
16295     end if 
16300 ! 
16305     if mode = inputattributesmode then ! #Select# Mode #Case# Inputattributesmode
16310       let window=fngetattributeswindow
16315       let fngetattributespec(mat inputspec$,mat inputdata$,mat inputttip$,mat inputsubs)
16320 ! 
16325       if control<>0 and srch(mat inputsubs,control)>0 then 
16330         let curfld(srch(mat inputsubs,control))
16335       end if 
16340 ! 
16345       let fnfiletoscreen(mat screenio$,mat screenio, mat inputdata$,mat inputsubs)
16350 ! 
16355       let fnadjustcolorbuttons(mat inputspec$,mat inputdata$,mat inputsubs)
16360 ! 
16365       let fncopyattrinputspec(mat inputonlyspec$,mat inputonlydata$,mat inputonlyttip$,mat inputspec$,mat inputdata$,mat inputttip$,mat inputsubs)
16370       print #window, fields mat inputspec$, help mat inputttip$ : mat inputdata$
16375 ! 
16380       execute "config keyboard 16 2200" ! Ctrl-V To Ctrl-F8 (Fkey 34)
16385       execute "config keyboard 0B00 0A0A0A636F6E206B657920636C6561720D" ! Shift F1 Key To "con key clear<CR>"
16390       do 
16395         rinput #window, fields mat inputonlyspec$, help mat inputonlyttip$ : mat inputonlydata$
16400         let io_screencodesubs=srch(mat inputsubs,si_screencode)
16405         let inputonlydata$(io_screencodesubs)=trim$(inputonlydata$(io_screencodesubs)) ! This keeps things from screwing up for leading spaces
16410 ! 
16415         if fkey=34 then 
16420           let clipboard$=env$("clipboard")
16425           if pos(clipboard$,"#!!") and pos(clipboard$,"_#_") then 
16430             if fnaddcontrolsfromclipboard(clipboard$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
16435 ! .                  ! Select new controls
16440               let oldsize=udim(mat selectedcontrols)
16445               mat selectedcontrols(udim(mat controlname$))=(0)
16450               mat selectedcontrols(oldsize+1:udim(mat selectedcontrols))=(1)
16455 ! 
16460               let control=oldsize+1
16465               let mode=inputeditormovemode
16470             end if 
16475           else 
16480             print #window, fields inputonlyspec$(curfld) : clipboard$(1:255) soflow IGNORECLIPBOARD
16485             let inputonlydata$(curfld)=clipboard$(1:255)
16490 IGNORECLIPBOARD: ! Continue
16495             let curfld(curfld,fkey)
16500           end if 
16505         end if 
16510       loop while fkey=34 and mode<>inputeditormovemode
16515       execute "config keyboard clear"
16520 ! 
16525       let fncopybackattrinputspec(mat inputonlyspec$,mat inputonlydata$,mat inputspec$,mat inputdata$,mat inputsubs)
16530       let fnscreentofile(mat screenio$, mat screenio, mat inputdata$, mat inputsubs)
16535 ! 
16540       if screenio(si_vsize)<1 then let screenio(si_vsize)=1
16545       if screenio(si_hsize)<1 then let screenio(si_hsize)=1
16550       if screenio(si_vsize)>120 then let screenio(si_vsize)=120
16555       if screenio(si_hsize)>220 then let screenio(si_hsize)=220
16560 ! 
16565       let control=inputsubs(curfld)
16570       let function=fkey
16575 ! 
16580     else if mode = inputfieldlistmode then ! #Case#  Inputfieldlistmode
16585       let window=fngetfieldswindow
16590       let fngetfieldsspec(inputspec$)
16595       rinput #window, fields inputspec$ : inputdata
16600       let function=fkey
16605 ! 
16610     else if mode = inputeditormode then ! #Case# Inputeditormode
16615       let function=fnpreformeditorinput(mode,control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
16620 ! 
16625     else if mode = inputeditormovemode then ! #Case# Inputeditormovemode
16630       let function=fnpreformobjectmovement(mode,control,mat screenio$,mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
16635 ! 
16640     else if mode = inputlistviewmode then ! #Case# Inputlistviewmode
16645       let function=fninputlistviewfields(mode,control,mat screenio$,mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
16650 ! 
16655     else if mode = inputdebugmode then ! #Case# Inputdebugmode
16660       let window=fngetdebugwindow
16665       let fngetdebugspec(inputspec$)
16670       rinput #window, fields inputspec$ : inputdata
16675       let function=fkey
16680 ! 
16685     else if mode = selectcolormode then ! #Case# Selectcolormode
16690       let tempcolor$=fncolorpicker$(screenio$(control),0,0,"Select Color",function)
16695       if fnvalidhexcolor(tempcolor$) or tempcolor$="" then 
16700         let screenio$(control)=tempcolor$
16705       end if 
16710       let mode=inputattributesmode
16715 ! 
16720     else if mode = selectfilelaymode then ! #Case# Selectfilelaymode
16725       let screenio$(control)=fnselectlayout$(screenio$(control),function)
16730       let mode=inputattributesmode
16735 ! 
16740     else if mode = selecteventsmode then ! #Case# Selecteventsmode
16745       let fnselectevents(mat screenio$,mat screenio,function)
16750       let mode=inputattributesmode
16755 ! 
16760     else if mode = settabordermode then ! #Case# Settabordermode
16765       let fnsettaborder(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,function)
16770       let mode=inputattributesmode
16775 ! 
16780     else if mode = configuredebugmode then ! #Case# Configuredebugmode
16785       let fnconfiguredebug(mat screenio$,mat screenio,function)
16790       let mode=inputattributesmode
16795 ! 
16800     else if mode = configureprotectedmode then ! #Case# ConfigureProtectedMode
16805       let fnconfigureprotected(mat screenio$,mat screenio,function)
16810       let mode=inputattributesmode
16815 ! 
16820     end if  ! #End Select#
16825     let fnpreforminput=function
16830 ! 
16835   fnend 
16840 ! 
16845 ADJUSTCOLORBUTTONS: ! Adjust The Colors Of The Buttons For The Attributes Window
16850   def fnadjustcolorbuttons(mat spec$,mat data$,mat subs;___,index)
16855     for index=1 to udim(mat subs)
16860       if subs(index) = si_fgcolor or subs(index) = si_bgcolor then ! #Select# Subs(Index) #Case# Si_Fgcolor # Si_Bgcolor
16865         let fnadjustspeccolor(spec$(index),data$(index))
16870       end if  ! #End Select#
16875     next index
16880   fnend 
16885 ! 
16890 COPYATTRINPUTSPEC: ! Copy The Attr Input Specs To The Input Spec Arrays
16895   def fncopyattrinputspec(mat inspec$,mat indata$, mat inttip$, mat allspec$, mat alldata$, mat allttip$, mat allsubs;___,index,count)
16900     mat inspec$(0) : mat indata$(0)
16905     for index=1 to udim(mat allsubs)
16910       if allsubs(index) = si_fgcolor or allsubs(index) = si_bgcolor or allsubs(index) = si_filelay or allsubs(index) = 0 then ! #Select# Allsubs(Index) #Case# Si_Fgcolor # Si_Bgcolor # Si_Filelay # 0
16915       else ! #Case Else#
16920         let count=udim(mat inspec$)+1
16925         mat inspec$(count) : mat indata$(count) : mat inttip$(count)
16930         let inspec$(count)=allspec$(index)
16935         let indata$(count)=alldata$(index)
16940         let inttip$(count)=allttip$(index)
16945       end if  ! #End Select#
16950     next index
16955   fnend 
16960 ! 
16965 COPYBACKATTRINPUTSPEC: ! Copy The Attr Input Specs Back To The All Spec Arrays
16970   def fncopybackattrinputspec(mat inspec$,mat indata$, mat allspec$, mat alldata$, mat allsubs;___,index,count)
16975     let count=0
16980     for index=1 to udim(mat allsubs)
16985       if allsubs(index) = si_fgcolor or allsubs(index) = si_bgcolor or allsubs(index) = si_filelay or allsubs(index) = 0 then ! #Select# Allsubs(Index) #Case# Si_Fgcolor # Si_Bgcolor # Si_Filelay # 0
16990       else ! #Case Else#
16995         let count+=1
17000         let allspec$(index)=inspec$(count)
17005         let alldata$(index)=indata$(count)
17010       end if  ! #End Select#
17015     next index
17020   fnend 
17025 ! 
17030 PROCESSCOMMAND: !  Process The User Command Here
17035   def fnprocesscommand(function,field,&mode,&control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,button$,menucomplete,newcontrol,relatedcontrol)
17040 ! 
17045     if function=98 then 
17050       let menucomplete = fnprocesswindowsmenu(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
17055       if menucomplete = menuquit then ! #Select# Menucomplete #Case# Menuquit
17060         let mode=quitmode
17065       else if menucomplete = menuload or menucomplete = menunew then ! #Case# Menuload # MenuNew
17070         let fncloseframewindows
17075         mat gaps=(0)
17080         let mode=inputattributesmode
17085         let control=1
17090       else if menucomplete = menuevents then ! #Case# Menuevents
17095         let mode=selecteventsmode
17100       else if menucomplete = menudebug then ! #Case# Menudebug
17105         let mode=configuredebugmode
17110       else if menucomplete = menuprotected then ! #Case# MenuProtected
17115         let mode=configureprotectedmode
17120       else if menucomplete = menutaborder then ! #Case# Menutaborder
17125         let mode=settabordermode
17130       else if menucomplete = menufgcolor then ! #Case# Menufgcolor
17135         let mode=selectcolormode
17140         let control=si_fgcolor
17145       else if menucomplete = menubgcolor then ! #Case# Menubgcolor
17150         let mode=selectcolormode
17155         let control=si_bgcolor
17160       else if menucomplete = menuselectlayout then ! #Case# Menuselectlayout
17165         let mode=selectfilelaymode
17170         let control=si_filelay
17175       else if menucomplete = menumovement then ! #Case# Menumovement
17180         let mode=inputeditormovemode
17185       else if menucomplete = menudebuglist then ! #Case# Menudebuglist
17190         let mode=inputdebugmode
17195       else if menucomplete = menufieldslist then ! #Case# Menufieldslist
17200         let mode=inputfieldslistmode
17205       else if menucomplete = menuaddcontrol then ! #Case# Menuaddcontrol
17210         let button$=menu$
17215       end if  ! #End Select#
17220     end if 
17225 ! 
17230     if function=93 or fkey=93 then 
17235       if fnoktoproceed(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
17240         let mode=quitmode
17245       end if 
17250     end if 
17255 ! 
17260     if function=99 then 
17265       mat selectedcontrols=(0)
17270       let mode=inputattributesmode
17275       let control=1
17280     end if 
17285 ! 
17290     if function>=1100 and function<1200 then !  Process Attributes Click
17295       let control = fnsetinputattributescontrol(function)
17300       if control = si_fgcolor or control = si_bgcolor then ! #Select# Control #Case# Si_Fgcolor # Si_Bgcolor
17305         let mode=selectcolormode
17310       else if control = si_filelay then ! #Case# Si_Filelay
17315         let mode=selectfilelaymode
17320       else if control = 0 then ! #Case# 0
17325         if function-1100=18 then 
17330           let mode=selecteventsmode
17335         else if function-1100=19 then 
17340           let mode=settabordermode
17345         end if 
17350       else ! #Case Else#
17355         let mode=inputattributesmode
17360       end if  ! #End Select#
17365     end if 
17370 ! 
17375     if function=1200 or (function=201 and mode=inputdebugmode) then !  Process Debug Click/Enter
17380       if mode=inputdebugmode then 
17385         let fnreaddebug(mode,control)
17390       else 
17395         let mode=inputdebugmode
17400         let control=0
17405       end if 
17410     end if 
17415 ! 
17420     if function=44 then ! Alt-Z Is Undo
17425       let fnundo(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
17430       mat selectedcontrols=(0)
17435     end if 
17440     if function=19 then ! Alt-R Is Redo
17445       let fnredo(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
17450     end if 
17455 ! 
17460     if function=1300 or (function=201 and mode=inputfieldlistmode) then !  Process Fieldslist Click/Enter
17465       if mode=inputfieldlistmode then 
17470         let newcontrol = fnaddandcolorcurrentfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat screenio$,mat screenio)
17475       else 
17480         let mode=inputfieldlistmode
17485         let control=0
17490       end if 
17495     end if 
17500 ! 
17505     if function>=1400 and function<1500 then !  Process Button
17510       let button$ = lwrc$(fnreadbutton$(function))
17515     end if 
17520 ! 
17525     if len(button$) then 
17530       let fnprocessbutton(button$,mode,control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
17535     end if 
17540 ! 
17545     if function>=1500 and function<=1500+udim(mat controlname$) then 
17550       if (control=function-1500) then 
17555         if (mode=inputeditormovemode) then 
17560           let mode=inputeditormode
17565         else if mode=inputeditormode and lwrc$(trim$(fieldtype$(control)))="listview" then 
17570           let mode=inputlistviewmode
17575         else 
17580           let mode=inputeditormovemode
17585         end if 
17590       else 
17595         let mode=inputeditormovemode
17600       end if 
17605       let control=function-1500
17610     end if 
17615   fnend 
17620 ! 
18000 !  #Autonumber# 18000,2
18002 ! *****************************************************************
18004 ! ************************ Set Tab Order **************************
18006 ! *****************************************************************
18008 SETTABORDER: ! Opens A Child Window To Select Event Handler Functions
18010   def fnsettaborder(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;&function,___,index,window)
18012     dim newtaborder(1)
18014     dim oldtabindex(1)
18016     dim newtaborderspec$(1)*255
18018 ! 
18020     let window=fngeteditorwindow
18022 ! 
18024     if window then 
18026       mat newtaborder(0)
18028       mat oldtabindex(0)
18030       mat newtaborderspec$(0)
18032       for index=1 to udim(mat fieldtype$)
18034         if fnisinput(fieldtype$(index)) then 
18036           mat newtaborder(udim(mat newtaborder)+1)
18038           let newtaborder(udim(mat newtaborder))=udim(mat newtaborder)
18040           mat newtaborderspec$(udim(mat newtaborder))
18042           let newtaborderspec$(udim(mat newtaborder))=str$(vposition(index))&","&str$(hposition(index))&","&str$(width(index))&"/NZ 3,S/#000000:#99CCFF,"&str$(1500+udim(mat newtaborder))
18044           mat oldtabindex(udim(mat newtaborder))
18046           let oldtabindex(udim(mat newtaborder))=index
18048         end if 
18050       next index
18052 ! 
18054       if udim(mat newtaborder) then 
18056         let index=0
18058 ! 
18060         let fnchangeforcevisibility(1)
18062         do 
18064           print #window, fields mat newtaborderspec$ : mat newtaborder
18066           input #0, fields "1,2,C 1,AEX" : key$
18068           let function=fkey
18070 ! 
18072           if function>1500 and function<=1500+udim(mat newtaborder) then 
18074             let index+=1
18076             let newtaborder(function-1500)=index
18078             let newtaborderspec$(function-1500)=newtaborderspec$(function-1500)(1:pos(newtaborderspec$(function-1500),",",-1)-1)&",-1"
18080             let newtaborderspec$(function-1500)=srep$(newtaborderspec$(function-1500),"#99CCFF","#00FF00")
18082           end if 
18084         loop until index=udim(mat newtaborder) or function=99 or function=98 or function=93 or function=44 or function=19 or ((function>1100) and (function<1500))
18086         let fnchangeforcevisibility(0)
18088 ! 
18090         if index=udim(mat newtaborder) then 
18092           let fnsetsortarrays(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18094           for index=1 to udim(mat newtaborder)
18096             let fngetsortarray(oldtabindex(index),oldtabindex(newtaborder(index)),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18098           next index
18100         end if 
18102       end if 
18104     end if 
18106   fnend 
18108 ! 
18110   dim srt_controlname$(1)*50, srt_fieldname$(1)*50, srt_description$(1)*255
18112   dim srt_vposition(1), srt_hposition(1), srt_fieldtype$(1)*8, srt_specwidth(1), srt_width(1), srt_height(1)
18114   dim srt_truevalue$(1)*60, srt_falsevalue$(1)*60
18116   dim srt_function$(1)*255, srt_picture$(1)*255, srt_parent$(1)*20
18118   dim srt_fgcolor$(1)*6, srt_bgcolor$(1)*6, srt_justify$(1)*1, srt_arrt$(1)*128, srt_multiselect(1), srt_gridlines(1)
18120   dim srt_protected(1), srt_invisible(1), srt_tooltip$(1)*255, srt_cnvrtin$(1)*255, srt_cnvrtout$(1)*255, srt_userdata$(1)*255
18122 ! 
18124 SETSORTARRAYS: !  Store The Controls In The Sort Arrays
18126   def fnsetsortarrays(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18128     mat srt_controlname$(udim(mat controlname$)) = controlname$
18130     mat srt_fieldname$(udim(mat fieldname$)) = fieldname$
18132     mat srt_description$(udim(mat description$)) = description$
18134     mat srt_vposition(udim(mat vposition)) = vposition
18136     mat srt_hposition(udim(mat hposition)) = hposition
18138     mat srt_fieldtype$(udim(mat fieldtype$)) = fieldtype$
18140     mat srt_specwidth(udim(mat specwidth)) = specwidth
18142     mat srt_width(udim(mat width)) = width
18144     mat srt_height(udim(mat height)) = height
18146     mat srt_truevalue$(udim(mat truevalue$)) = truevalue$
18148     mat srt_falsevalue$(udim(mat falsevalue$)) = falsevalue$
18150     mat srt_function$(udim(mat function$)) = function$
18152     mat srt_picture$(udim(mat picture$)) = picture$
18154     mat srt_parent$(udim(mat parent$)) = parent$
18156     mat srt_fgcolor$(udim(mat fgcolor$)) = fgcolor$
18158     mat srt_bgcolor$(udim(mat bgcolor$)) = bgcolor$
18160     mat srt_justify$(udim(mat justify$)) = justify$
18162     mat srt_attr$(udim(mat attr$)) = attr$
18164     mat srt_protected(udim(mat protected)) = protected
18166     mat srt_invisible(udim(mat invisible)) = invisible
18168     mat srt_tooltip$(udim(mat tooltip$)) = tooltip$
18170     mat srt_cnvrtin$(udim(mat cnvrtin$)) = cnvrtin$
18172     mat srt_cnvrtout$(udim(mat cnvrtout$)) = cnvrtout$
18174     mat srt_multiselect(udim(mat multiselect)) = multiselect
18176     mat srt_gridlines(udim(mat gridlines)) = gridlines
18178     mat srt_userdata$(udim(mat userdata$))=userdata$
18180   fnend 
18182 ! 
18184 GETSORTARRAY: ! Sort The Selected Control Out Of The Sort Arrays
18186   def fngetsortarray(from,to,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18188     let controlname$(to) = srt_controlname$(from)
18190     let fieldname$(to) = srt_fieldname$(from)
18192     let description$(to) = srt_description$(from)
18194     let vposition(to) = srt_vposition(from)
18196     let hposition(to) = srt_hposition(from)
18198     let fieldtype$(to) = srt_fieldtype$(from)
18200     let specwidth(to) = srt_specwidth(from)
18202     let width(to) = srt_width(from)
18204     let height(to) = srt_height(from)
18206     let truevalue$(to) = srt_truevalue$(from)
18208     let falsevalue$(to) = srt_falsevalue$(from)
18210     let function$(to) = srt_function$(from)
18212     let picture$(to) = srt_picture$(from)
18214     let parent$(to) = srt_parent$(from)
18216     let fgcolor$(to) = srt_fgcolor$(from)
18218     let bgcolor$(to) = srt_bgcolor$(from)
18220     let justify$(to) = srt_justify$(from)
18222     let attr$(to) = srt_attr$(from)
18224     let protected(to) = srt_protected(from)
18226     let invisible(to) = srt_invisible(from)
18228     let tooltip$(to) = srt_tooltip$(from)
18230     let cnvrtin$(to) = srt_cnvrtin$(from)
18232     let cnvrtout$(to) = srt_cnvrtout$(from)
18234     let multiselect(to) = srt_multiselect(from)
18236     let gridlines(to) = srt_gridlines(from)
18238     let userdata$(to) = srt_userdata$(from)
18240   fnend 
18242 ! 
18244 ! *****************************************************************
18246 ! ************************* Move Control **************************
18248 ! *****************************************************************
18250   dim dotsprinted, settingtempdots
18252   dim clipboard$*20000
18254 PREFORMOBJECTMOVEMENT: ! Highlight An Object And Allow Them To Move It Around.
18256   def fnpreformobjectmovement(&mode, &control, mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,newv,newh,neww,newhth,key$,function,th,tv,exitmovement,wwindow,relatedcontrol,index,oldsize,oldsetting)
18258     if control<1 then let control=1
18260     if control>udim(mat fieldtype$) then let control=udim(mat fieldtype$)
18262     if control>0 then 
18264       let wwindow=fngeteditorwindow
18266 ! 
18268       if wwindow then 
18270         let newv=vposition(control) : let newh=hposition(control)
18272         let neww=width(control) : let newhth=height(control)
18274 ! 
18276         execute "config keyboard 0B 0F00" ! Up To Shift F5  (Fkey 15)
18278         execute "config keyboard 0A 1000" ! Dn To Shift F6  (Fkey 16)
18280         execute "config keyboard 0E 1100" ! Lf To Shift F7  (Fkey 17)
18282         execute "config keyboard 0C 1200" ! Rt To Shift F8  (Fkey 18)
18284         execute "config keyboard 08 2000" ! <- To (Fkey 32) - 19 Is Taken
18286         execute "config keyboard 20 1400" ! Sp To Shift F10 (Fkey 20)
18288         execute "config keyboard 04 1500" ! Del To Shift F11 (Fkey 21)
18290 ! 
18292         execute "config keyboard 05 1600" ! Ctrl-E To Shift-F12 (Fkey 22)
18294         execute "config keyboard 12 1700" ! Ctrl-R To Ctrl-F3 (Fkey 23)
18296         execute "config keyboard 17 1800" ! Ctrl-W To Ctrl-F4 (Fkey 24)
18298         execute "config keyboard 13 1900" ! Ctrl-S To Ctrl-F5 (Fkey 25)
18300         execute "config keyboard 03 2100" ! Ctrl-C To (Fkey 33)
18302         execute "config keyboard 16 2200" ! Ctrl-V To (Fkey 34)
18304         execute "config keyboard 18 2300" ! Ctrl-X To (Fkey 35)
18306         execute "config keyboard 14 2400" ! Ctrl-T To (Fkey 36)
18308 ! 
18310         execute "config keyboard 0B00 0A0A0A636F6E206B657920636C6561720D" ! Shift F1 Key To "con key clear<CR>"
18312         execute "config keyboard 0C00 676F20737465700D" ! Shift F2 Key To "go step<CR>"
18314         execute "config keyboard 0D00 20" ! Shift F3 -> Space
18316 ! 
18318         mat selectedcontrols(udim(mat fieldtype$))
18320 ! 
18322         let fnselectframecontrols(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18324         do 
18326           mat alreadymoved(udim(mat selectedcontrols))=(0)
18328           let fnmovecontrol(control,newv,newh,neww,newhth,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,0,mat selectedcontrols)
18330 ! 
18332           if ~dotsprinted then 
18334             let scr_freeze
18336             let fnchangeforcevisibility(1)
18338             let fnprintdots(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18340             let dotsprinted=1
18342           end if 
18344 ! 
18346           let fnchangeforcevisibility(0)
18348 ! 
18350           input #0, fields "1,2,C 1,AEX" : key$
18352 ! 
18354           let function=fkey
18356 ! 
18358           if function = 99 then ! #Select# Function #Case# 99
18360             if sum(mat selectedcontrols) then 
18362               mat selectedcontrols=(0)
18364               let fnrenderscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18366               let function=1000 ! Do Nothing, Stay Here.
18368             end if 
18370 ! 
18372           else if function = 5 then ! #Case# 5
18374 ! .               ! F5 - Refresh
18376             let fnrenderscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18378             let fnchangeforcevisibility(1)
18380             let fnprintdots(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18382             let dotsprinted=1
18384             let fnchangeforcevisibility(0)
18386 ! 
18388           else if function = 6 then ! #Case# 6
18390 ! .               ! F6 - RedrawDots
18392             let settingtempdots=1
18394             let fnrenderscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18396             let fnchangeforcevisibility(1)
18398             let fnprintdots(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18400             let dotsprinted=1
18402             let fnchangeforcevisibility(0)
18404 ! 
18406           else if function = 33 then ! #Case# 33
18408 ! .               ! Copy
18410             let clipboard$=""
18412             if udim(mat selectedcontrols)>=control and selectedcontrols(control) then 
18414               for relatedcontrol=1 to udim(mat selectedcontrols)
18416                 if selectedcontrols(relatedcontrol) then 
18418                   let fnaddtoclipboard(clipboard$,relatedcontrol,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) ! Ctrl-C
18420                 end if 
18422               next relatedcontrol
18424             else 
18426               let fnaddtoclipboard(clipboard$,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18428             end if 
18430             let setenv("clipboard",clipboard$)
18432 ! 
18434           else if function = 34 then ! #Case# 34
18436 ! .               ! Paste
18438             let clipboard$=env$("clipboard")
18440             if fnaddcontrolsfromclipboard(clipboard$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
18442 ! .                  ! Select new controls
18444               let oldsize=udim(mat selectedcontrols)
18446               mat selectedcontrols(udim(mat controlname$))=(0)
18448               mat selectedcontrols(oldsize+1:udim(mat selectedcontrols))=(1)
18450 ! 
18452               let control=oldsize+1
18454               let function=1001 ! Exit To Mainloop But Come Back In
18456             end if 
18458 ! 
18460           else if function = 35 then ! #Case# 35
18462 ! .               ! Cut: Copy and Delete
18464             let clipboard$=""
18466             if udim(mat selectedcontrols)>=control and selectedcontrols(control) then 
18468               for relatedcontrol=1 to udim(mat selectedcontrols)
18470                 if selectedcontrols(relatedcontrol) then 
18472                   let fnaddtoclipboard(clipboard$,relatedcontrol,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) ! Ctrl-C
18474                 end if 
18476               next relatedcontrol
18478             else 
18480               let fnaddtoclipboard(clipboard$,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18482             end if 
18484             let setenv("clipboard",clipboard$)
18486 ! 
18488 ! .               ! Delete
18490             let fncheckpoint(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18492             let control=fndeletecontrol(control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat selectedcontrols) ! Delete Control And Assign New Contrl
18494             if control then 
18496               let newv=vposition(control)
18498               let newh=hposition(control)
18500               let neww=width(control)
18502               let newhth=height(control)
18504               let fncheckpoint(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18506             else 
18508               let function=99 ! Escape Out Of Movement Mode
18510             end if 
18512           else if function = 15 then ! #Case# 15
18514             let newv=vposition(control)-1 !  Up
18516           else if function = 16 then ! #Case# 16
18518             let newv=vposition(control)+1 !  Down
18520           else if function = 17 then ! #Case# 17
18522             let newh=hposition(control)-1 ! Left
18524           else if function = 18 then ! #Case# 18
18526             let newh=hposition(control)+1 !  Right
18528           else if function = 32 then ! #Case# 32
18530             let neww=width(control)-1 !  <- Is Shorter
18532             if justify$(control)="R" then ! Shrink from the left
18534               let newh=hposition(control)+1 ! so move it right
18536             end if 
18538           else if function = 20 then ! #Case# 20
18540             let neww=width(control)+1 !  Space Is Wider
18542             if justify$(control)="R" then ! Grow on the left
18544               let newh=hposition(control)-1 ! so move it left
18546             end if 
18548           else if function = 90 then ! #Case# 90
18550             let newhth=height(control)-1 ! Pgup Is Shorter
18552           else if function = 91 then ! #Case# 91
18554             let newhth=height(control)+1 ! Pgdn Is Taller
18556           else if function = 21 then ! #Case# 21
18558 ! .               ! Delete
18560             let fncheckpoint(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18562             let control=fndeletecontrol(control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat selectedcontrols) ! Delete Control And Assign New Contrl
18564             if control then 
18566               let newv=vposition(control)
18568               let newh=hposition(control)
18570               let neww=width(control)
18572               let newhth=height(control)
18574               let fncheckpoint(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18576             else 
18578               let function=99 ! Escape Out Of Movement Mode
18580             end if 
18582 ! 
18584           else if function = 0 then ! #Case# 0
18586             let function=1500+control ! Enter Is Same As Click
18588 ! 
18590           else if function = 22 then ! #Case# 22
18592 ! .               ! Ctrl-E: Make all Left Edges of selected controls match Yellow Control
18594             mat alreadymoved(udim(mat selectedcontrols))=(0)
18596             for relatedcontrol=1 to udim(mat selectedcontrols)
18598               if selectedcontrols(relatedcontrol) and relatedcontrol<>control then 
18600                 let fnmovecontrol(relatedcontrol,vposition(relatedcontrol),hposition(control),width(relatedcontrol),height(relatedcontrol),mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18602               end if 
18604             next relatedcontrol
18606             let fnrenderscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18608 ! 
18610           else if function = 23 then ! #Case# 23
18612 ! .               ! Ctrl-R: Make all Right Edges of Selected Controls match Yellow Control
18614             mat alreadymoved(udim(mat selectedcontrols))=(0)
18616             for relatedcontrol=1 to udim(mat selectedcontrols)
18618               if selectedcontrols(relatedcontrol) and relatedcontrol<>control then 
18620                 let fnmovecontrol(relatedcontrol,vposition(relatedcontrol),max(1,hposition(control)+width(control)-width(relatedcontrol)),width(relatedcontrol),height(relatedcontrol),mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18622               end if 
18624             next relatedcontrol
18626             let fnrenderscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18628 ! 
18630           else if function = 24 then ! #Case# 24
18632 ! .               ! Ctrl-W: Make all widths of selected controls match yellow control
18634             mat alreadymoved(udim(mat selectedcontrols))=(0)
18636             for relatedcontrol=1 to udim(mat selectedcontrols)
18638               if selectedcontrols(relatedcontrol) and relatedcontrol<>control then 
18640                 let fnmovecontrol(relatedcontrol,vposition(relatedcontrol),hposition(relatedcontrol),width(control),height(relatedcontrol),mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18642               end if 
18644             next relatedcontrol
18646             let fnrenderscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18648 ! 
18650           else if function = 25 then ! #Case# 25
18652 ! .               ! Ctrl-S: Toggle Selection
18654             mat selectedcontrols(udim(mat controlname$))
18656             let selectedcontrols(control)=~selectedcontrols(control)
18658 ! 
18660           else if function = 36 then ! #Case# 36
18662 ! .               ! Ctrl-T: Select All Textboxes
18664             mat selectedcontrols(udim(mat controlname$))=(0)
18666             for relatedcontrol=1 to udim(mat fieldtype$)
18668               if lwrc$(trim$(fieldtype$(relatedcontrol)))="c" then 
18670                 let selectedcontrols(relatedcontrol)=1
18672               end if 
18674             next relatedcontrol
18676             let fnrenderscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18678 ! 
18680           end if  ! #End Select#
18682 ! 
18684           if function>2000 then 
18686             let fngetclickposition(function,tv, th)
18688             if ((tv>0) and (th>0)) then 
18690               let newv=tv
18692               let newh=th
18694             end if 
18696           end if 
18698 ! 
18700           if (function>1100 and function<1500) or function=1001 or function=98 or function=99 or function=93 or function=44 or function=19 or function=1500+control then 
18702             let exitmovement=1
18704           end if 
18706 ! 
18708           if function=110 or function=111 or (function>1500 and function<=1500+udim(mat controlname$) and function<>1500+control) then 
18710 ! 
18712             let fnchangeforcevisibility(1)
18714             let fndrawcontrol(wwindow,control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0) ! Inactivate Old Control
18716 ! 
18718             if (relatedcontrol:=fnfindrelatedcontrol(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)) then 
18720               let fndrawcontrol(wwindow,relatedcontrol,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0) ! Inactivate Old Caption
18722             end if 
18724 ! 
18726             if lwrc$(trim$(fieldtype$(control)))="frame" then 
18728               let fnprintdots(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,vposition(control),hposition(control),width(control)+(2*(lwrc$(trim$(fieldtype$(control)))=="combo")),height(control))
18730               let fnredrawscreenpart(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,vposition(control),hposition(control),width(control)+(2*(lwrc$(trim$(fieldtype$(control)))=="combo")),height(control))
18732             end if 
18734 ! 
18736             let fnchangeforcevisibility(0)
18738 ! 
18740             if function=110 then 
18742               let control+=1
18744               if control>udim(mat controlname$) then let control=1
18746             else if function=111 then 
18748               let control-=1
18750               if ~control then let control=udim(mat controlname$)
18752             else 
18754               let control=function-1500
18756             end if 
18758             let newv=vposition(control)
18760             let newh=hposition(control)
18762             let neww=width(control)
18764             let newhth=height(control)
18766 ! 
18768             let fnselectframecontrols(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18770 ! 
18772             let fncheckpoint(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
18774           end if 
18776 ! 
18778 ! .             ! Standardize Heights
18780         loop until exitmovement
18782         let scr_freeze
18784         execute "config keyboard clear" !  Clear Keyboard Remapping
18786         let fnpreformobjectmovement=function
18788       end if 
18790     else 
18792       let mode=inputattributesmode
18794     end if 
18796   fnend 
18798 ! 
18800   dim alreadymoved(1)
18802   dim zero(1)
18804 MOVECONTROL: ! Move A Control And Output  The Specs To Move It
18806   def fnmovecontrol(index,newv,newh,neww,newhth,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;dontactive,donteraseyet,mat selcont,___,weditor,relatedindex,eraserow,erasecol,erasewidth,eraseheight,dotsize,oldv,oldh,oldwth,oldhth,olderaserow,olderasecol,leftdistance,rightdistance,topdistance,bottomdistance,selindex,bordersize)
18808     let weditor=fngeteditorwindow
18810 ! 
18812 !      Let Msgbox("Move Control "&Str$(Index)&":"&Str$(Vposition(Index))&","&Str$(Newv)&":"&Str$(Hposition(Index))&","&Str$(Newh))
18814     if ~alreadymoved(index) then 
18816 ! 
18818 !         ! If things are selected and we're moving the selection then
18820 !         !  we need to make sure that the distance from here to the bottommost control is also
18822 !         !  considered.
18824 ! 
18826 !         ! This code is designed to ensure that controls dont bunch up
18828 !         ! when moving groups of them near the edge with the mouse
18830 !         !  but alas.. it didn't work so its commented out now.
18832 ! 
18834 !         let TopDistance=0 : let BottomDistance=0
18836 !         let LeftDistance=0 : let RightDistance=0
18838 !         if Udim(Mat Selcont)>=Index And selcont(Index) then
18840 !            for selIndex=1 to udim(mat selcont)
18842 !               if selcont(selIndex) then
18844 !                  let topdistance=max(topdistance,VPosition(Index)-Vposition(selIndex))
18846 !                  let bottomdistance=max(bottomdistance,(VPosition(selIndex)+Height(selIndex)-1)-(VPosition(Index)+Height(Index)-1))
18848 !                  let leftdistance=max(leftdistance,HPosition(Index)-HPosition(selIndex))
18850 !                  let rightDistance=max(Rightdistance,(HPosition(selIndex)+Width(selIndex)-1)-(HPosition(Index)+Width(Index)-1))
18852 !               end if
18854 !            next selIndex
18856 !         end if
18858 ! 
18860 !         let Newv=Max(Min(Newv,1+Screenio(Si_Vsize)-Max(Newhth,1)+BottomDistance),1+TopDistance)
18862 !         let Newh=Max(Min(Newh,1+Screenio(Si_Hsize)-Neww+RightDistance),1+LeftDistance)
18864 ! 
18866       if (lwrc$(trim$(fieldtype$(index)))="frame" or lwrc$(trim$(fieldtype$(index)))="screen") and gridlines(index) then let bordersize=1
18868 ! 
18870       if lwrc$(trim$(fieldtype$(index)))<>"screen" then 
18872         let neww=max(neww,1)
18874         let neww=min(screenio(si_hsize)-2*bordersize,neww)
18876         let newhth=max(newhth,1)
18878         let newhth=min(screenio(si_vsize)-2*bordersize,newhth)
18880       end if 
18882 ! 
18884       let newv=max(min(newv,1+screenio(si_vsize)-max(newhth,1)-bordersize),1+bordersize)
18886       let newh=max(min(newh,1+screenio(si_hsize)-max(neww,1)-bordersize),1+bordersize)
18888 ! 
18890       if lwrc$(trim$(fieldtype$(index))) = "listview" or lwrc$(trim$(fieldtype$(index))) = "p" or lwrc$(trim$(fieldtype$(index))) = "frame" or lwrc$(trim$(fieldtype$(index))) = "caption" or lwrc$(trim$(fieldtype$(index))) = "c" or lwrc$(trim$(fieldtype$(index))) = "button" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "listview"#"p"#"frame"#"caption"#"c"#"button"
18892         let newhth=max(1,newhth)
18894       else if lwrc$(trim$(fieldtype$(index))) = "screen" then ! #Case# "screen"
18896 ! .         ! Child Screen Controls can't change size.
18898         let newhth=height(index)
18900         let neww=width(index)
18902       else ! #Case Else#
18904         let newhth=1
18906       end if  ! #End Select#
18908 ! 
18910       if newv<>vposition(index) or newh<>hposition(index) or neww<>width(index) or newhth<>height(index) then 
18912         let eraserow=vposition(index)
18914         let erasecol=hposition(index)
18916         let erasewidth=width(index)+(2*(lwrc$(trim$(fieldtype$(index)))=="combo"))
18918         let eraseheight=max(height(index),1)
18920 ! 
18922         if (relatedindex:=fnfindrelatedcontrol(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)) then 
18924           let fnmovecontrol(relatedindex,vposition(relatedindex)+newv-vposition(index),hposition(relatedindex)+newh-hposition(index),width(relatedindex),height(relatedindex),mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
18926         end if 
18928 ! .         ! Move controls inside frame if frame moves.
18930         if lwrc$(trim$(fieldtype$(index)))="frame" then 
18932           if (newv<>vposition(index) or newh<>hposition(index)) then 
18934             for relatedindex=1 to udim(mat framemovement)
18936               if framemovement(relatedindex) then 
18938                 let fnmovecontrol(relatedindex,vposition(relatedindex)+newv-vposition(index),hposition(relatedindex)+newh-hposition(index),width(relatedindex)+neww-width(index),height(relatedindex)+newhth-height(index),mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1,1)
18940               end if 
18942             next relatedindex
18944           end if 
18946         end if 
18948         if udim(mat selcont)>=index and selcont(index) then 
18950           let oldv=vposition(index)
18952           let oldh=hposition(index)
18954           let oldwth=width(index)
18956           let oldhth=height(index)
18958           for relatedindex=1 to udim(mat selcont)
18960             if selcont(relatedindex) then 
18962 ! .                  ! Expand EraseRow and EraseCol to include new control...
18964               let olderaserow=eraserow
18966               let olderasecol=erasecol
18968               let eraserow=min(eraserow,vposition(relatedindex))
18970               let erasecol =min(erasecol,hposition(relatedindex))
18972               let erasewidth=max(olderasecol+erasewidth,hposition(relatedindex)+width(relatedindex)+(2*(lwrc$(trim$(fieldtype$(relatedindex)))=="combo")))-erasecol
18974               let eraseheight=max(olderaserow+eraseheight,vposition(relatedindex)+max(1,height(relatedindex)))-eraserow
18976 ! 
18978               let fnmovecontrol(relatedindex,vposition(relatedindex)+newv-oldv,hposition(relatedindex)+newh-oldh,width(relatedindex)+neww-oldwth,height(relatedindex)+newhth-oldhth,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1,1)
18980             end if 
18982           next relatedindex
18984         end if 
18986       end if 
18988 ! 
18990       let vposition(index)=newv
18992       let hposition(index)=newh
18994       let width(index)=neww
18996       let height(index)=newhth
18998 ! 
19000       if lwrc$(trim$(fieldtype$(index))) = "frame" then ! #Select# lwrc$(trim$(fieldType$(index))) #case# "frame"
19002       else if lwrc$(trim$(fieldtype$(index))) = "c" or lwrc$(trim$(fieldtype$(index))) = "button" or lwrc$(trim$(fieldtype$(index))) = "textbox" or lwrc$(trim$(fieldtype$(index))) = "caption" then ! #Case# "c" # "button" # "textbox" # "caption"
19004         if height(index)<=1 then 
19006           let fnchangeforcevisibility(1)
19008         end if 
19010       else ! #Case Else#
19012         let fnchangeforcevisibility(1)
19014       end if  ! #End Select#
19016 ! 
19018       if eraserow and erasecol and erasewidth and eraseheight and ~donteraseyet then 
19020         let dotsize=fncalculatedotsize(screenio(si_vsize),screenio(si_hsize))
19022         let fnprintdots(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,eraserow,erasecol,erasewidth,eraseheight) ! Erase Old Control
19024         if dotsize>1 then 
19026           let fnprintdots(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,vposition(index),hposition(index),width(index)+(2*(lwrc$(trim$(fieldtype$(index)))=="combo")),max(height(index),1))
19028         end if 
19030         let fnredrawscreenpart(weditor,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,eraserow,erasecol,erasewidth,eraseheight)
19032       end if 
19034 ! 
19036       let fndrawcontrol(weditor,index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,~dontactive)
19038 ! 
19040       if lwrc$(trim$(fieldtype$(index)))="frame" then 
19042         let fnchangeforcevisibility(1)
19044         let fnredrawscreenpart(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,vposition(control),hposition(control),width(control)+(2*(lwrc$(trim$(fieldtype$(control)))=="combo")),height(control))
19046       end if 
19048 ! 
19050       let fnchangeforcevisibility(0)
19052       let alreadymoved(index)=1
19054     end if 
19056   fnend 
19058 ! 
19060   def fnredrawscreenpart(weditor,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,eraserow,erasecol,erasewidth,eraseheight;___,checkcontrol)
19062     for checkcontrol=1 to udim(mat controlname$)
19064       if lwrc$(trim$(fieldtype$(checkcontrol)))<>"frame" and lwrc$(trim$(fieldtype$(checkcontrol)))<>"listchld" then 
19066         if ((vposition(checkcontrol)+max(height(checkcontrol),1)-1 >= eraserow) and (vposition(checkcontrol)<=eraserow+eraseheight-1)) then 
19068           if ((hposition(checkcontrol)+width(checkcontrol)+(2*(lwrc$(trim$(fieldtype$(checkcontrol)))=="combo"))-1 >= erasecol) and (hposition(checkcontrol)<=erasecol+erasewidth-1)) then 
19070             let fndrawcontrol(weditor,checkcontrol,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
19072           end if 
19074         end if 
19076       end if 
19078     next checkcontrol
19080   fnend 
19082 ! 
19084   dim framemovement(1)
19086   def fnselectframecontrols(frame,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
19088     mat framemovement(udim(mat fieldtype$))
19090     if lwrc$(trim$(fieldtype$(frame)))="frame" then 
19092       for index=1 to udim(mat fieldtype$)
19094         if index<>frame then 
19096           if ((vposition(index)>=vposition(frame)) and (vposition(index)+max(height(index),1)<=vposition(frame)+height(frame))) then 
19098             if ((hposition(index)>=hposition(frame)) and (hposition(index)+width(index)<=hposition(frame)+width(frame))) then 
19100               let framemovement(index)=1
19102             end if 
19104           end if 
19106         end if 
19108       next index
19110     else 
19112       mat framemovement=(0)
19114     end if 
19116   fnend 
19118 ! 
19120 DELETECONTROL: ! Delete The Given Control And Return A New Control
19122   def fndeletecontrol(control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;mat selcont,___,index)
19124     if udim(mat selcont)>=control and selcont(control) then 
19126       let index=0
19128       do while index<udim(mat selcont)
19130         let index+=1
19132         if selcont(index) then 
19134           let fndelete(index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
19136           if index<udim(mat selcont) then 
19138             mat selcont(index:udim(mat selcont)-1)=selcont(index+1:udim(mat selcont))
19140           end if 
19142           mat selcont(udim(mat selcont)-1)
19144           let index-=1
19146         end if 
19148       loop while udim(mat selcont)
19150     else 
19152       let fndelete(control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
19154     end if 
19156     let fnrenderscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
19158 ! 
19160     if control>udim(mat controlname$) then let control=udim(mat controlname$)
19162     let fndeletecontrol=control
19164   fnend 
19166 ! 
19168   def fndelete(control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,row,col,width,height,childsearch)
19170     let row=vposition(control)
19172     let col=hposition(control)
19174     let width=width(control)+(2*(lwrc$(trim$(fieldtype$(control)))=="combo"))
19176     let height=height(control)
19178 ! 
19180     if lwrc$(trim$(fieldtype$(control))) = "listview" then ! #Select# Lwrc$(Trim$(Fieldtype$(Control))) #Case# "listview"
19182 ! .       ! If its a listview, then we need to delete all its children too.
19184       for childsearch=1 to udim(mat fieldtype$)
19186         if childsearch<=udim(mat fieldtype$) then 
19188           if lwrc$(trim$(fieldtype$(childsearch)))="listchld" and parent$(childsearch)=parent$(control) then 
19190             for index=childsearch to udim(mat controlname$)-1
19192               let fncopycontrolarrays(index,index+1,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
19194             next index
19196             let fnresizecontrolarrays(udim(mat controlname$)-1,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
19198           end if 
19200         end if 
19202       next childsearch
19204     else if lwrc$(trim$(fieldtype$(control))) = "frame" or lwrc$(trim$(fieldtype$(control))) = "c" or lwrc$(trim$(fieldtype$(control))) = "button" or lwrc$(trim$(fieldtype$(control))) = "caption" then ! #Case# "frame" # "c" # "button" # "caption"
19206 ! .      ! if it has a frame, we need to close the window
19208       let fncloseframewindow(parent$(control))
19210     else if lwrc$(trim$(fieldtype$(control))) = "screen" then ! #Case# "screen"
19212 ! .      ! if its a screen, we need to close it
19214       let childsearch=srch(mat screenkeys$,parent$(control))
19216       if childsearch>0 then 
19218         if screenwindows(childsearch) and file(childsearch)<>-1 then close #screenwindows(childsearch): 
19220         for index=childsearch to udim(mat screenkeys$)-1
19222           let screenkeys$(index)=screenkeys$(index+1)
19224           let screenwindows(index)=screenwindows(index+1)
19226         next index
19228         mat screenkeys$(udim(mat screenkeys$)-1)
19230         mat screenwindows(udim(mat screenwindows)-1)
19232       end if 
19234 ! 
19236     end if  ! #End Select#
19238 ! 
19240     for index=control to udim(mat controlname$)-1
19242       let fncopycontrolarrays(index,index+1,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
19244     next index
19246     let fnresizecontrolarrays(udim(mat controlname$)-1,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
19248 ! 
19250     let fnchangeforcevisibility(1)
19252     let fnprintdots(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,row,col,width,height)
19254     let fnchangeforcevisibility(0)
19256   fnend 
19258 ! 
19260 ! 
19262 FINDRELATEDCONTROL: ! Finds The Related Control If Any
19264   def fnfindrelatedcontrol(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,relatedindex)
19266     if trim$(lwrc$(fieldtype$(control))) = "c" or trim$(lwrc$(fieldtype$(control))) = "search" or trim$(lwrc$(fieldtype$(control))) = "filter" or trim$(lwrc$(fieldtype$(control))) = "combo" then ! #Select# Trim$(Lwrc$(Fieldtype$(Control))) #Case# "c" # "search" # "filter" # "combo"
19268       if trim$(description$(control))<>"" then 
19270         if (relatedindex:=srch(mat controlname$,description$(control))) > 0 then 
19272           let fnfindrelatedcontrol=relatedindex
19274         end if 
19276       end if 
19278     end if  ! #End Select#
19280   fnend 
19282 ! 
19284 ! 
20000 !  #Autonumber# 20000,5
20005 ! *****************************************************************
20010 ! ****************************** Dots *****************************
20015 ! *****************************************************************
20020 ADDDOT: ! Adds A Dot To The Dots Arrays
20025   def fnadddot(row,col,size,mat dotrow,mat dotcol,mat dotsize,mat dotexist;___,index)
20030     let index=srch(mat dotexist,0)
20035     if index<1 then 
20040       let index=udim(mat dotrow) + 1
20045       mat dotrow(index)
20050       mat dotcol(index)
20055       mat dotsize(index)
20060       mat dotexist(index)
20065     end if 
20070     let dotrow(index)=row
20075     let dotcol(index)=col
20080     let dotsize(index)=size
20085     let dotexist(index)=1
20090 ! 
20095     let fnadddot=index
20100   fnend 
20105 ! 
20110 MASKDOT: ! Masks Out A Dot From The Dots Arrays
20115   def fnmaskdot(row,col,mat dotrow,mat dotcol,mat dotsize,mat dotexist;___,dot,index,jndex)
20120     let dot=fnfinddot(row,col,mat dotrow,mat dotcol,mat dotsize,mat dotexist)
20125     if dot then 
20130       for index=dotrow(dot) to dotrow(dot)+dotsize(dot)-1
20135         for jndex=dotcol(dot) to dotcol(dot)+dotsize(dot)-1
20140           if index<>row or jndex<>col then 
20145             let fnadddot(index,jndex,1,mat dotrow,mat dotcol,mat dotsize,mat dotexist) ! Add Smaller Dots In The Old Positions
20150           end if 
20155         next jndex
20160       next index
20165 ! 
20170       let dotrow(dot)=0 ! Delete Dot
20175       let dotcol(dot)=0
20180       let dotsize(dot)=0
20185       let dotexist(dot)=0
20190     end if 
20195   fnend 
20200 ! 
20205 FINDDOT: ! Find Which Dot Exists On A Given Row
20210   def fnfinddot(row,col,mat dotrow,mat dotcol,mat dotsize,mat dotexist;___,index,dotfound)
20215     do while (index:=srch(mat dotexist,1,index+1))>0
20220       if index>0 then 
20225         if (row>=dotrow(index)) and (row<=dotrow(index)+dotsize(index)-1) then 
20230           if (col>=dotcol(index)) and (col<=dotcol(index)+dotsize(index)-1) then 
20235             let dotfound=1 ! Found
20240             exit do 
20245           end if 
20250         end if 
20255       end if 
20260     loop 
20265 ! 
20270     if dotfound then 
20275       let fnfinddot=index
20280     end if 
20285   fnend 
20290 ! 
20295 PRINTDOTS: ! Print The Clickable Dots For Click-To-Move Functionality
20300   def fnprintdots(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;maskrow,maskcol,maskwidth,maskheight,___,index,row,col,dotsize,tr,tc,printfullscreen,window)
20305     dim dotrow(1), dotcol(1), dotsize(1),dotexist(1)
20310 ! 
20315     let window=fngeteditorwindow
20320 ! 
20325     if window>0 then 
20330       mat dotrow(0) : mat dotcol(0) : mat dotsize(0) : mat dotexist(0)
20335 ! 
20340       let dotsize=fncalculatedotsize(screenio(si_vsize),screenio(si_hsize))
20345 ! 
20350       if ~(maskrow or maskcol or maskwidth or maskheight) then 
20355         let printfullscreen=1
20360       end if 
20365 ! 
20370       if maskrow and maskcol and maskwidth and ~maskheight then 
20375         let maskheight=1
20380       end if 
20385 ! 
20390       if settingtempdots or pos(uprc$(env$("clickmove")),"Y") then 
20395 !    .      Assign Default Mask Area
20400         if ~maskrow then let maskrow=1
20405         if ~maskcol then let maskcol=1
20410         if ~maskwidth then let maskwidth=screenio(si_hsize)-maskcol+1
20415         if ~maskheight then let maskheight=screenio(si_vsize)-maskrow+1
20420 ! 
20425 !    .      Grow Mask Area Slightly To Include Only Whole Dots
20430         let maskheight+=(maskrow-((int((maskrow-1)/dotsize)*dotsize)+1))
20435         let maskwidth+=(maskcol-((int((maskcol-1)/dotsize)*dotsize)+1))
20440         let maskrow=(int((maskrow-1)/dotsize)*dotsize)+1
20445         let maskcol=(int((maskcol-1)/dotsize)*dotsize)+1
20450         let maskheight=int(((maskheight-1)/dotsize)+1)*dotsize
20455         let maskwidth=int(((maskwidth-1)/dotsize)+1)*dotsize
20460 ! 
20465 !    .      Build Dots Only For Mask Area
20470         for row=maskrow to maskrow+maskheight-1 step dotsize
20475           for col=maskcol to maskcol+maskwidth-1 step dotsize
20480             let fnadddot(row,col,dotsize,mat dotrow,mat dotcol,mat dotsize,mat dotexist)
20485           next col
20490         next row
20495 ! 
20500 !    .      Finally, Mask Off Any Dots That May Be Outside The Screen.
20505         for col=screenio(si_hsize)+1 to maskcol+maskwidth
20510           for row=maskrow to maskrow+maskheight
20515             let fnmaskdot(row,col,mat dotrow,mat dotcol,mat dotsize,mat dotexist)
20520           next row
20525         next col
20530         for row=screenio(si_vsize)+1 to maskrow+maskheight
20535           for col=maskcol to maskcol+maskwidth
20540             let fnmaskdot(row,col,mat dotrow,mat dotcol,mat dotsize,mat dotexist)
20545           next col
20550         next row
20555 ! 
20560 !    .      Mask Controls Only If They Fall Inside The Already Defined Mask Area.
20565         for index=1 to udim(controlname$)
20570           if lwrc$(trim$(fieldtype$(index)))<>"listchld" and lwrc$(trim$(fieldtype$(index)))<>"frame" then 
20575             for row=vposition(index) to (vposition(index)+max(1,height(index))-1)
20580               for col=hposition(index) to (hposition(index)+width(index)-1)+(2*(lwrc$(trim$(fieldtype$(index)))=="combo"))
20585                 if col<=maskcol+maskwidth and col>=maskcol and row<=maskrow+maskheight and row>=maskrow then 
20590                   let fnmaskdot(row,col,mat dotrow,mat dotcol,mat dotsize,mat dotexist)
20595                 end if 
20600               next col
20605             next row
20610           end if 
20615         next index
20620 ! 
20625         let fnrenderdots(window,dotsize,mat dotrow,mat dotcol,mat dotsize,mat dotexist,printfullscreen)
20630       else 
20635         if ~printfullscreen then 
20640           for index=maskrow to maskrow+max(maskheight,1)-1
20645             print #window, fields str$(index)&","&str$(maskcol)&",C "&str$(maskwidth) : ""
20650           next index
20655         end if 
20660       end if 
20665     end if 
20670   fnend 
20675 ! 
20680   dim permdotrow(1), permdotcol(1), permdotsize(1), permdotexist(1)
20685 RENDERDOTS: ! Move The Dots To The Permenant Dot Arrays And Print Them
20690   def fnrenderdots(window,dotsize,mat dotrow,mat dotcol,mat dotsize,mat dotexist;clearpermanent,___,index,dot,row,col,fkeyvalue)
20695     dim dotspec$(1)*40, dotdata$(1)*60
20700     mat dotspec$(0) : mat dotdata$(0)
20705 ! 
20710     if clearpermanent then 
20715       mat permdotrow(0) : mat permdotcol(0) : mat permdotsize(0) : mat permdotexist(0)
20720     end if 
20725 ! 
20730     mat dotspec$(sum(mat dotexist))
20735     mat dotdata$(sum(mat dotexist))
20740 ! 
20745     let index=0
20750     do while (index:=srch(mat dotexist,1,index+1))>0
20755       if ~clearpermanent then 
20760         for row=dotrow(index) to dotrow(index)+dotsize(index)-1
20765           for col=dotcol(index) to dotcol(index)+dotsize(index)-1
20770             let fnmaskdot(row,col,mat permdotrow,mat permdotcol,mat permdotsize,mat permdotexist)
20775           next col
20780         next row
20785       end if 
20790       let fkeyvalue=fnadddot(dotrow(index),dotcol(index),dotsize(index),mat permdotrow,mat permdotcol,mat permdotsize,mat permdotexist)
20795 ! 
20800       let dot+=1
20805       let dotspec$(dot)=str$(dotrow(index))&","&str$(dotcol(index))&",P "&str$(dotsize(index))&"/"&str$(dotsize(index))&",,"&str$(2000+fkeyvalue)
20810       if dotsize(index)<dotsize then 
20815         let dotdata$(dot)=setting_imagepath$&"\black.bmp"
20820       else 
20825         if dotsize=1 then 
20830           let dotdata$(dot)=setting_imagepath$&"\dot.bmp"
20835         else 
20840           let dotdata$(dot)=setting_imagepath$&"\bigdot.bmp"
20845         end if 
20850       end if 
20855     loop 
20860 ! 
20865     print #window, fields mat dotspec$ : mat dotdata$
20870   fnend 
20875 ! 
20880 CALCULATEDOTSIZE: ! Calculate The Step Size Based On The Screen Size
20885   def fncalculatedotsize(vsize,hsize;___,buttonsize,maxnumberofbuttons)
20890     let maxnumberofbuttons = 3000
20895 ! 
20900     let buttonsize=int((((vsize*hsize)-1)/maxnumberofbuttons))+1
20905     let buttonsize=int(sqr(buttonsize-1))+1
20910 ! 
20915     let fncalculatedotsize = buttonsize
20920   fnend 
20925 ! 
20930 GETCLICKPOSITION: ! Returns The Click Position
20935   def fngetclickposition(function,&row,&col;___,index)
20940     if function>2000 then 
20945       let row=permdotrow(function-2000)
20950       let col=permdotcol(function-2000)
20955     end if 
20960   fnend 
20965 ! 
21000 !  #Autonumber# 21000,2
21002 ! *****************************************************************
21004 ! ************************ Render Controls ************************
21006 ! *****************************************************************
21008   dim draw_lv_caption$(1)*255,draw_lv_width(1),draw_lv_spec$(1)*40,draw_lv_data$(1)*255
21010 DRAWCONTROL: ! Draw The Given Control
21012   def fndrawcontrol(window,control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;active,fgcolor$,bgcolor$,dontpopulate,forceindex,runtime,___,data$*255,spec$*255,index)
21014     if width(control)<=screenio(si_hsize) and height(control)<=screenio(si_vsize) then 
21016       if lwrc$(trim$(fieldtype$(control))) = "listview" then ! #Select# Lwrc$(Trim$(Fieldtype$(Control)))   #Case# "listview"
21018 ! .          ! Draw Listview Header
21020         let spec$=fncalculatelistviewheaders$(mat draw_lv_caption$, mat draw_lv_width, mat draw_lv_spec$,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,active)
21022         if len(trim$(fgcolor$)) and len(trim$(bgcolor$)) then 
21024           let spec$=srep$(spec$,fncolor$(0,fieldtype$(control),fgcolor$(control),bgcolor$(control),0,invisible(control)),fncolor$(0,fieldtype$(control),fgcolor$,bgcolor$))
21026         end if 
21028 ! 
21030 ! .         ! This next part fixes it so that if you have a Read event for your listview, the other stuff all updates correctly.
21032 ! .         !  In order for ScreenIO to do its magic, there has to be an "X" attribute for the listview and for all other input
21034 ! .         !  text controls on the screen, so that it can get control and make sure things are updated properly and the Read Event runs.
21036         if len(trim$(screenio$(si_readfn))) then 
21038           for index=1 to udim(mat draw_lv_spec$)
21040             if pos(draw_lv_spec$(index),",") and pos(draw_lv_spec$(index),"/",pos(draw_lv_spec$(index),",",-1)) then 
21042               let draw_lv_spec$(index)=draw_lv_spec$(index)(1:pos(draw_lv_spec$(index),"/",-1)-1)&"X"&draw_lv_spec$(index)(pos(draw_lv_spec$(index),"/",-1):len(draw_lv_spec$(index)))
21044             else 
21046               let draw_lv_spec$(index)=draw_lv_spec$(index)&"X"
21048             end if 
21050           next index
21052           for index=1 to udim(mat fieldtype$)
21054             if fnisinput(fieldtype$(index)) then 
21056               if pos(attr$(index),"/") then 
21058                 if attr$(index)(pos(attr$(index),"/")-1:pos(attr$(index),"/")-1)><"X" then 
21060                   let attr$(index)=attr$(index)(1:pos(attr$(index),"/")-1)&"X"&attr$(index)(pos(attr$(index),"/"):len(attr$(index)))
21062                 end if 
21064               else 
21066                 if attr$(index)(len(attr$(index)):len(attr$(index)))><"X" then 
21068                   let attr$(index)=attr$(index)&"X"
21070                 end if 
21072               end if 
21074             end if 
21076           next index
21078         end if 
21080 ! 
21082         if forceindex then 
21084           let spec$=spec$(1:pos(spec$,",",-1)-1)&","&str$(forceindex)
21086         end if 
21088 ! 
21090 ! .         ! Add Hidden Column for Record Number
21092         mat draw_lv_caption$(udim(mat draw_lv_caption$)+1)
21094         mat draw_lv_width(udim(mat draw_lv_width)+1)
21096         mat draw_lv_spec$(udim(mat draw_lv_spec$)+1)
21098 ! 
21100         let draw_lv_width(udim(mat draw_lv_width))=0
21102         let draw_lv_spec$(udim(mat draw_lv_spec$))="C 12"
21104 ! 
21106 ! .         ! Add Hidden Column for Search
21108 !            mat Draw_Lv_Caption$(Udim(Mat Draw_Lv_Caption$)+1)
21110 !            mat Draw_Lv_Width(Udim(Mat Draw_Lv_Width)+1)
21112 !            mat Draw_Lv_Spec$(Udim(Mat Draw_Lv_Spec$)+1)
21114 ! 
21116 !            let Draw_Lv_Width(Udim(Mat Draw_Lv_Width))=0
21118 !            let Draw_Lv_Spec$(Udim(Mat Draw_Lv_Spec$))="C 1000"
21120 ! 
21122         print #window, fields spec$ : (mat draw_lv_caption$, mat draw_lv_width, mat draw_lv_spec$)
21124 ! 
21126 ! .         ! Populate Listview with Empty Data
21128         mat draw_lv_data$((height(control)-1)*udim(draw_lv_width))=("")
21130         let spec$=fncalculatelistviewspec$(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
21132         print #window, fields spec$&",=" : (mat draw_lv_data$)
21134         if ~dontpopulate then 
21136           let fntrytopopulatelistviews(window,mat screenio$,mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,control)
21138         end if 
21140       else if lwrc$(trim$(fieldtype$(control))) = "listchld" then ! #Case# "listchld"
21142 ! .         ! Listview Children get drawn by the Draw Listview Code Above
21144       else if lwrc$(trim$(fieldtype$(control))) = "frame" then ! #Case# "frame"
21146         let spec$=fncalculateframespec$(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,active,runtime)
21148         if len(trim$(fgcolor$)) and len(trim$(bgcolor$)) then 
21150           let spec$=srep$(spec$,fncolor$(0,fieldtype$(control),fgcolor$(control),bgcolor$(control),0,invisible(control)),fncolor$(0,fieldtype$(control),fgcolor$,bgcolor$))
21152         end if 
21154         let fnopenframewindow(window,spec$,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,runtime,forceindex)
21156 ! 
21158       else if lwrc$(trim$(fieldtype$(control))) = "screen" then ! #Case# "screen"
21160         let fnopenscreen(window,spec$,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,runtime,forceindex,active)
21162 ! 
21164       else ! #Case Else#
21166         let data$=fncalculatedata$(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,active)
21168         let spec$=fncalculatespec$(window,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,active,1)
21170 ! 
21172         if len(trim$(fgcolor$)) and len(trim$(bgcolor$)) then 
21174           let spec$=srep$(spec$,fncolor$(0,fieldtype$(control),fgcolor$(control),bgcolor$(control),0,invisible(control)),fncolor$(0,fieldtype$(control),fgcolor$,bgcolor$))
21176         end if 
21178         print #window, fields spec$ : data$ ! Inactivate Old Control
21180       end if  ! #End Select#
21182     end if 
21184   fnend 
21186 ! 
21188   def fncloseframewindows(;___,index)
21190     let fnchangeforcevisibility(0)
21192     for index=1 to udim(mat framewindows)
21194       if framewindows(index) and file(framewindows(index))<>-1 then 
21196         close #framewindows(index): 
21198       end if 
21200     next index
21202     mat framewindows(0)
21204     mat framekeys$(0)
21206     mat framespeccontrol$(0)
21208     for index=1 to udim(mat screenwindows)
21210       if screenwindows(index) and file(screenwindows(index))<>-1 then 
21212         close #screenwindows(index): 
21214       end if 
21216     next index
21218     mat screenwindows(0)
21220     mat screenkeys$(0)
21222   fnend 
21224 ! 
21226 ! .! if framespec is given, don't close that one.
21228   def fncloseframewindow(framekey$;&framespec$,___,framekey)
21230 ! .   ! check to see if window is already open
21232     let framekey=srch(mat framekeys$,framekey$)
21234     if framekey>0 then 
21236       if ~len(framespec$) or framespeccontrol$(framekey)><framespec$ then 
21238         if framewindows(framekey) and file(framewindows(framekey))<>-1 then 
21240 ! .            ! print "Close "&FrameKey$ : pause
21242           close #framewindows(framekey): 
21244           let framewindows(framekey)=0
21246           let framespeccontrol$(framekey)=""
21248         end if 
21250       end if 
21252     end if 
21254   fnend 
21256 ! 
21258   def fnopenframewindow(window,spec$*255,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;runtime,forceindex,___,framekey,alreadyopen)
21260     if trim$(parent$(control))="" then let parent$(control)=fngenerateuniquelvname$(mat parent$,fieldtype$(control))
21262 ! .   ! check to see if window is already open
21264     let framekey=srch(mat framekeys$,parent$(control))
21266     if framekey>0 then 
21268       if framewindows(framekey) and file(framewindows(framekey))<>-1 then 
21270         if spec$><framespeccontrol$(framekey) then 
21272           close #framewindows(framekey): 
21274           let framewindows(framekey)=0
21276         else 
21278           let alreadyopen=1
21280         end if 
21282       end if 
21284     else 
21286       let framekey=udim(mat framewindows)
21288       if ~framekey or framewindows(framekey) then 
21290         let framekey+=1
21292         mat framewindows(framekey)
21294         mat framekeys$(framekey)
21296         mat framespeccontrol$(framekey)
21298       end if 
21300       let framekeys$(framekey)=parent$(control)
21302       let framespeccontrol$(framekey)=spec$
21304     end if 
21306     if (~alreadyopen) and (~runtime or ~invisible(control)) then 
21308       let framewindows(framekey)=fngetfilenumber(-1)
21310       open #framewindows(framekey): spec$&",parent="&str$(window),display,outin 
21312 ! .      ! print "Open "&Parent$(Control) : pause
21314       if forceindex then 
21316         print #framewindows(framekey), fields "1,1,C "&str$(width(control)*height(control))&",/W:T,"&str$(forceindex) : "" ! Make it hot
21318       else if ~runtime then 
21320         print #framewindows(framekey), fields "1,1,C "&str$(width(control)*height(control))&",/W:T,"&fnmakeindex$(fieldtype$(control),control) : "" ! Make it hot
21322       end if 
21324     end if 
21326   fnend 
21328 ! 
21330   dim screenwindows(1)
21332   dim screenkeys$(1)
21334 ! 
21336   dim pdata$(1)*255
21338 ! 
21340   def fnopenscreen(window,spec$*255,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;runtime,forceindex,active,_path$*255,___,screenkey,keyval$*255,pth$*255,parent_key$*255,recordval,dummy$*255,dummy,windownumber,record)
21342 ! .   ! check to see if window is already open
21344     let screenkey=srch(mat screenkeys$,parent$(control))
21346     if screenkey>0 then 
21348       if screenwindows(screenkey) and file(screenwindows(screenkey))<>-1 then 
21350         close #screenwindows(screenkey): 
21352         let screenwindows(screenkey)=0
21354       end if 
21356     else 
21358       let screenkey=udim(mat screenwindows)
21360       if ~screenkey or screenwindows(screenkey) then 
21362         let screenkey+=1
21364         mat screenwindows(screenkey)
21366         mat screenkeys$(screenkey)
21368       end if 
21370       let screenkeys$(screenkey)=parent$(control)
21372     end if 
21374     if ~runtime or ~invisible(control) then 
21376       if ~forceindex then let forceindex=control+fnkeybase
21378       if len(trim$(_path$)) then let pth$=_path$ else let pth$=path$
21380       if ~runtime then 
21382         let fnpushscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
21384         let parentkey$=screenio$(si_debugparentkey)
21386         let currentkey$=screenio$(si_debugkey)
21388         let currentrec=screenio(si_debugrecord)
21390         let pth$=screenio$(si_debugpath)
21392         let fnunpackpdata$(mat pdata$,screenio$(si_debugpassed))
21394       end if 
21396       let dummy$="["&fieldname$(control)&"]"&function$(control)
21398       if runtime then let fnparsescreeninfo(dummy$,keyval$,parent_key$,dummy,dummy,dummy,dummy,dummy,recordval,parentkey$)
21400       let windownumber=val(fnmaster$(fieldname$(control),keyval$,vposition(control),hposition(control),parent_key$,window,1,1,recordval,pth$,0,active,forceindex,~runtime,mat pdata$))
21402       if ~runtime then 
21404         let fnpopscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
21406       end if 
21408       let screenwindows(screenkey)=windownumber
21410     end if 
21412   fnend 
21414 ! 
21416   def fnunpackpdata$(mat x$,y$*255;___,index)
21418     if fn42 then 
21420       let str2mat(y$,mat x$,",")
21422     else 
21424       mat x$(0)
21426       do while pos(y$,",")
21428         let index=udim(mat x$)+1
21430         mat x$(index)
21432         let x$(index)=y$(1:pos(y$,",")-1)
21434         let y$(1:pos(y$,","))=""
21436       loop 
21438       if len(trim$(y$)) then 
21440         let index=udim(mat x$)+1
21442         mat x$(index)
21444         let x$(index)=y$
21446       end if 
21448     end if 
21450   fnend 
21452 ! 
21454 DRAWSCREENS: ! Predraw all the screens on the screen
21456   def fndrawscreens(window,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;runtime,forceindex,whichone,___,index)
21458     for index=1 to udim(mat fieldtype$) ! Look at all controls
21460       if whichone>=0 or whichone*-1=index then ! Draw just the ones we want
21462         if lwrc$(trim$(fieldtype$(index))) = "screen" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "screen"
21464           if fnvalidspec(index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
21466             let fndrawcontrol(window,index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,"","",0,forceindex,runtime)
21468           end if 
21470         end if  ! #End Select#
21472       end if 
21474     next index
21476   fnend 
21478 ! 
21480 DRAWFRAMES: ! Predraw all the frames on the screen
21482   def fndrawframes(window,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;runtime,forceindex,whichone,___,index)
21484     for index=1 to udim(mat fieldtype$) ! Look at all controls
21486       if whichone>=0 or whichone*-1=index then ! Draw just the ones we want
21488         if lwrc$(trim$(fieldtype$(index))) = "frame" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "frame"
21490           if fnvalidspec(index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
21492             let fndrawcontrol(window,index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,"","",0,forceindex,runtime)
21494           end if 
21496         end if  ! #End Select#
21498       end if 
21500     next index
21502   fnend 
21504 ! 
21506 DRAWALLLISTVIEWS: ! Redraw All The Listviews On The Screen
21508   def fndrawalllistviews(window,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;runtime,forceindex,___,index)
21510     for index=1 to udim(mat fieldtype$) ! Draw All Listview Controls (there's only ever one)
21512       if ~runtime or ~invisible(index) then 
21514         if lwrc$(trim$(fieldtype$(index))) = "listview" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "listview"
21516           if fnvalidspec(index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
21518             let fndrawcontrol(window,index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,"","",1,forceindex)
21520           end if 
21522         end if  ! #End Select#
21524       end if 
21526     next index
21528   fnend 
21530 ! 
21532 TRYTOPOPULATELISTVIEWS: ! Populates The Listview With Sample Data If Possible
21534   def fntrytopopulatelistviews(window,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;justthisone,___,index,prefix$,filenumber,theresalistview)
21536     dim trypopulate$(1)*1000, trypopulate(1)
21538     for index=1 to udim(mat fieldtype$)
21540       if lwrc$(fieldtype$(index))="listview" then 
21542         let theresalistview=1
21544       end if 
21546     next index
21548 ! 
21550     if theresalistview and pos(uprc$(env$("poplist")),"Y") and trim$(screenio$(si_filelay))<>"" and fndoeslayoutexist(trim$(screenio$(si_filelay))) then 
21552       let filenumber=fnopen(trim$(screenio$(si_filelay)),mat trypopulate$,mat trypopulate,mat form$)
21554 ! 
21556       let fnreadlayoutarrays(trim$(screenio$(si_filelay)),prefix$,mat fieldsssubs$,mat fieldsnsubs$)
21558       for index=1 to udim(mat fieldsssubs$)
21560         let fieldsssubs$(index)=lwrc$(trim$(fieldsssubs$(index)))
21562       next index
21564       for index=1 to udim(mat fieldsnsubs$)
21566         let fieldsnsubs$(index)=lwrc$(trim$(fieldsnsubs$(index)))
21568       next index
21570 ! 
21572       if pos(uprc$(env$("filter")),"Y") and (~librarylinkage or routinename$<>screenio$(si_screencode)) then 
21574         let routinename$=screenio$(si_screencode)
21576         let fnestablishlibrarylinkage
21578         let fnreadlayoutarrays("screenio",screenioprefix$,mat screeniossubs$,mat screenionsubs$)
21580         let fnexecutesetsubscripts(mat controlname$,"ctl_")
21582         let fnexecuteuniquesubscripts(mat controlname$,"ctl_")
21584       end if 
21586 ! 
21588       let fninitializemats(mat s$,mat screensubs$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
21590       let fnexecutesetsubscripts(mat screensubs$,"sio_")
21592 ! 
21594       let fnpopulatealllistviews(window,filenumber,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat form$,mat listviewrecords,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,justthisone,(~(librarylinkage and pos(uprc$(env$("filter")),"Y"))),1)
21596       let fnclosefile(filenumber,trim$(screenio$(si_filelay)),"",1)
21598     end if 
21600   fnend 
21602 ! 
21604 ! 
25000 ! #Autonumber# 25000, 5
25005 POPULATEALLLISTVIEWS: ! This Function Populates All The Listviews On The Screen
25010   def fnpopulatealllistviews(window,fdatafile,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat form$,mat records,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;justthisone,ignorefilters,editing,&firstrow,displayonly,___,attribute$*128,index,spec$*60,columns,col,populaterow,colindex,position,color$*40,screensubindex,clearflag$,populatedataudim,populatecolorudim,populatesize,savefdatafile,fileform,rowcount,every10)
25015 ! 
25020     dim populatedata$(1000)*1000
25025     dim populatecolor$(9999)*40
25030     dim populateread$(1)*1000
25035     dim populateread(1)
25040     dim listviewchildindex(1)
25045     dim populatesortedsubs
25050 ! 
25055 !      dim SearchColumn$*1000
25060 ! 
25065     if fdatafile and file(fdatafile)>-1 then ! If File Is Open
25070 ! 
25075       let populatesize=9999
25080 ! .!
25085       mat populateread$(udim(mat fieldsssubs$))
25090       mat populateread(udim(mat fieldsnsubs$))
25095 ! 
25100       if ~ignorefilters then 
25105         if exists(setting_functionfolder$&"defaults\") then 
25110           if exists(setting_functionfolder$&"defaults\prelist.brs") then 
25115             let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\prelist.brs"))
25120           end if 
25125         end if 
25130 ! 
25135         if len(trim$(screenio$(si_prelistviewfn))) then 
25140           let fnexecute(trim$(screenio$(si_prelistviewfn)))
25145         end if 
25150       end if 
25155 ! .!
25160       for index=1 to udim(mat fieldtype$)
25165         if (index=justthisone) or (not justthisone) then 
25170           if editing or ~invisible(index) then 
25175             if lwrc$(trim$(fieldtype$(index))) = "listview" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "listview"
25180               if fnvalidspec(index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
25185 ! 
25190                 let fileform=fdatafile
25195 ! 
25200                 if ~screenio(si_readindex) then 
25205 ! .                        ! Open file relative for faster reading
25210                   let savefdatafile=fdatafile
25215                   open #(fdatafile:=fngetfilenumber(400)): "name="&file$(savefdatafile)&",shr",internal,input,relative 
25220                 end if 
25225 ! 
25230                 let fnprepareanim
25235 ! 
25240                 let populatedataudim=0
25245                 let populatecolorudim=0
25250                 let fnclearcolorarrays
25255 ! 
25260                 let clearflag$=",="
25265                 let columns=fncountcolumns(parent$(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
25270                 mat listviewchildindex(columns)
25275                 for col=1 to columns
25280                   let listviewchildindex(col)=fnfindlistviewchild(col,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
25285                 next col
25290                 restore #fdatafile: 
25295                 let spec$=fncalculatelistviewspec$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
25300 ! 
25305                 if fnvalidattribute(attr$(index),fieldtype$(index)) then 
25310                   let attribute$=attr$(index)
25315                 end if 
25320 ! .   !
25325                 let rowcount=0
25330 ! 
25335                 if ~ignorefilters and len(trim$(function$(index))) then 
25340                   let fnexecute("{{SetData}}")
25345                 end if 
25350 ! 
25355                 dim populateincrement,populatereckeysize
25360                 let populateincrement=1000 : let populatereckeysize=0
25365                 mat records(0)
25370 ! 
25375                 do 
25380                   read #fdatafile, using form$(fileform), release: mat populateread$, mat populateread error ignore
25505                   if file(fdatafile)=0 then ! If No Read Problems
25510                     if ~ignorefilters then 
25515                       let color$:=fnpassfilter$(mat populateread$,mat populateread,function$(index),index)
25520                     end if 
25525                     if ignorefilters or (len(trim$(color$)) and trim$(color$)<>"0" and uprc$(trim$(color$))<>"STOP") then 
25530                       let rowcount+=1
25535                       let populatecolorudim+=1
25540                       if populatecolorudim>25000 then 
25545                         let fnsetcolors(mat populatecolor$,mat lvac_start,mat lvac_end,mat lvac_color$)
25550                         mat populatecolor$=("")
25555                         let populatecolorudim=1
25560                       end if 
25565                       if populatesize<populatecolorudim then 
25570                         mat populatecolor$(25000)
25575                         let populatesize=25000
25580                       end if 
25585                       let populatecolor$(populatecolorudim)=color$
25590                       let populaterow=populatedataudim
25595                       let populatedataudim+=(columns+1) ! Extra Columns To Store Record Number
25600 ! 
25605                       if rowcount>populatereckeysize then ! Ensure its big enough
25610                         let populatereckeysize+=populateincrement
25615                         mat records(populatereckeysize)
25620                       end if 
25625 ! .   !
25630                       let records(rowcount)=rec(datafile)
25635 ! 
25640 !                                 let SearchColumn$=""
25645                       for col=1 to columns
25650                         let columnindex=listviewchildindex(col)
25655                         if trim$(fieldname$(columnindex))="" and trim$(controlname$(columnindex))<>"" and (screensubindex:=srch(mat screensubs$,lwrc$(trim$(controlname$(columnindex)))))>0 then 
25660                           let populatedata$(populaterow+col)=s$(screensubindex)
25665                         else 
25670                           if fnisnumber(mat fieldsssubs$,mat fieldsnsubs$,fieldname$(columnindex)) then 
25675                             let position=srch(mat fieldsnsubs$,lwrc$(trim$(fieldname$(columnindex))))
25680                             if position>0 then 
25685                               let populatedata$(populaterow+col)=str$(populateread(position))
25690                             end if 
25695                           else 
25700                             let position=srch(mat fieldsssubs$,lwrc$(trim$(fieldname$(columnindex))))
25705                             if position>0 then 
25710                               let populatedata$(populaterow+col)=populateread$(position)
25715                               let fntrimtocrlf(populatedata$(populaterow+col))
25720                             end if 
25725                           end if 
25730                         end if 
25735                         if fnapplyconversion(cnvrtin$(columnindex)) then 
25740                           let populatedata$(populaterow+col)=cnvrt$(trim$(cnvrtin$(columnindex)),val(populatedata$(populaterow+col))) error ignore
25745                         end if  ! Attempt To Apply Conversion Manually If Fnapplyconversion Thinks We Should
25750 ! .                                 ! let SearchColumn$=SearchColumn$&"|"&PopulateData$(PopulateRow+Col)
25755                         let populatedata$(populaterow+col)=populatedata$(populaterow+col)(1:specwidth(columnindex))
25760                       next col
25765                       let populatedata$(populaterow+columns+1)=str$(rec(fdatafile))
25770 ! .                              ! let Populatedata$(Populaterow+Columns+2)=SearchColumn$
25775                       if (populatedataudim+columns+2)>1000 then 
25780                         mat populatedata$(populatedataudim)
25785                         print #window, fields spec$&clearflag$&attribute$ : (mat populatedata$)
25790                         mat populatedata$(1000)
25795                         let populatedataudim=0
25800                         let clearflag$=",+"
25805                       end if 
25810                       let fnrunanim
25815                     end if 
25820                   end if 
25825                 loop until (file(fdatafile)=10) or (uprc$(trim$(color$))="STOP") or (editing and (populatedataudim>((height(index)+5)*(columns+1))))
25830 ! 
25835                 if ~ignorefilters and len(trim$(function$(index))) then 
25840                   let fnexecute("{{GetData}}")
25845                 end if 
25850 ! 
25855                 if ~populatecolorudim then 
25860                   let populatedataudim=columns+1
25865                   mat populatedata$(1:populatedataudim)=("")
25870                 end if 
25875 ! 
25880                 let fncloseanim ! close animation window if its there.
25885 ! 
25890                 mat populatedata$(populatedataudim)
25895                 print #window, fields spec$&clearflag$&attribute$ : (mat populatedata$)
25900                 mat populatedata$(1000)
25905                 if populatecolorudim then 
25910                   mat populatecolor$(populatecolorudim)
25915                   let fnsetcolors(mat populatecolor$,mat lvac_start,mat lvac_end,mat lvac_color$)
25920                   mat populatecolor$(25000)
25925                   let populatecolorudim=0
25930 ! 
25935                   let fnapplycolor(window,spec$)
25940                   if specwidth(index)<>0 then 
25945                     if specwidth(index)>0 then 
25950                       print #window, fields spec$&",SORT" : specwidth(index)
25955                     else if specwidth(index)<0 then 
25960                       print #window, fields spec$&",SORT" : abs(specwidth(index))
25965                       print #window, fields spec$&",SORT" : abs(specwidth(index))
25970                     end if 
25975                     if ~editing and ~displayonly and fn43 then 
25980                       mat populatesortedsubs(rowcount)
25985                       input #window, fields spec$&",ROWSUB,ALL,DISPLAYED_ORDER,NOWAIT" : mat populatesortedsubs
25990                       let firstrow=populatesortedsubs(1)
25995                     end if 
26000                   end if 
26005 ! 
26010                   if fn42 and gridlines(index) then 
26015                     print #window, fields spec$&",GRIDLINES" : 1
26020                   end if 
26025                 end if 
26030                 mat records(rowcount)
26035                 if savefdatafile then close #fdatafile: : let fdatafile=savefdatafile
26040                 let fnpopulatealllistviews=rowcount
26045               end if 
26050             else ! #Case Else#
26055 ! .   ! .            ! Ignore Non-Listview Controls
26060             end if  ! #End Select#
26065           end if 
26070         end if 
26075       next index
26080 ! 
26085       if ~ignorefilters then 
26090 ! 
26095         if exists(setting_functionfolder$&"defaults\") then 
26100           if exists(setting_functionfolder$&"defaults\postlist.brs") then 
26105             let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\postlist.brs"))
26110           end if 
26115         end if 
26120 ! 
26125         if len(trim$(screenio$(si_postlistviewfn))) then 
26130           let fnexecute(trim$(screenio$(si_postlistviewfn)))
26135         end if 
26140       end if 
26145 ! 
26150     end if 
26155   fnend 
26160 ! 
26500 !  #Autonumber# 26500,2
26502   def fntrimtocrlf(&string$;___,pos0d,pos0a,poscrlf)
26504     let pos0d=pos(string$,hex$("0D"))
26506     let pos0a=pos(string$,hex$("0A"))
26508     if pos0d and pos0a then 
26510       let poscrlf=min(pos0d,pos0a)
26512     else if pos0d then 
26514       let poscrlf=pos0d
26516     else if pos0a then 
26518       let poscrlf=pos0a
26520     else 
26522       let poscrlf=0
26524     end if 
26526     if poscrlf then let string$=string$(1:poscrlf-1)
26528   fnend 
26530 ! 
26532   def fnisnumeric(&string$;___,dummy)
26534     let dummy=val(string$) conv NOTNUMERIC
26536     let fnisnumeric=1
26538 NOTNUMERIC: ! Do Nothing
26540   fnend 
26542 ! 
26544 ! .! Strip up to third comma, for Listview Spec (Row,Col,LIST Rows/Cols,)
26546   def library fnlistspec$*255(specin$*255)=specin$(1:pos(specin$,",",pos(specin$,",",pos(specin$,",")+1)+1)-1)
26548 ! 
26550 ! .! Animation Functionality
26552   def library fnprepareanimation=fnprepareanim
26554   def library fnanimate(;text$*60)=fnrunanim(text$)
26556   def library fncloseanimation=fncloseanim
26558 ! 
26560   def fnprepareanim ! set defaults
26562     dim animtiming,animation,animwindow,cspeed,animframe,animv,animh,arw,acl,cbd$*40,ccl$*40,arows,atext
26564     let animtiming=animation=animwindow=cspeed=animframe=animv=animh=arw=acl=0
26566     let cbd$=ccl$=""
26568     let cspeed=.5
26570 ! 
26572     let fnsettings
26574   fnend 
26576 ! 
26578   dim animspec$(1)
26580 ! 
26582   def fnrunanim(;&text$)
26584 ! .   ! Note: if text is given for any animation call,
26586 ! .   !  it must be given for the first one.
26588     if animation<>-1 then 
26590       if timer-animtiming>cspeed then 
26592         let animtiming=timer
26594 ! 
26596         if animation or (animation:=fnreadclockfolder(setting_clockpath$,mat animation$,cspeed,animv,animh,ccl$,cbd$))>0 then 
26598 ! .            ! Open Animation Window if its not open
26600           if ~animwindow then 
26602             dim clockattr$*128
26604             let clockattr$=""
26606 ! 
26608             let fnreadscreensize(screenv,screenh)
26610             if len(trim$(ccl$)) then let clockattr$=clockattr$&",N="&ccl$
26612             if len(trim$(cbd$)) then let clockattr$=clockattr$&",Border="&cbd$
26614             let arows=animv
26616             if len(text$) then 
26618               let arows+=1
26620               let atext=1
26622               let animh=max(len(text$),animh)
26624               mat animspec$(2)
26626               let animspec$(2)=str$(arows)&",1,"&str$(animh)&"/CC 60"
26628             else 
26630               mat animspec$(1)
26632               let atext=0
26634             end if 
26636             let animspec$(1)="1,1,P "&str$(animv)&"/"&str$(animh)
26638             let arw=int((screenv-animv)/2)+1
26640             let acl=int((screenh-animh)/2)+1
26642             open #(animwindow:=fngetfilenumber(-1)): "srow="&str$(arw)&",scol="&str$(acl)&",rows="&str$(arows)&",cols="&str$(animh)&clockattr$,display,outin 
26644           end if 
26646 ! 
26648           let animframe+=1
26650           if animframe>udim(mat animation$) then let animframe=1
26652           if atext then 
26654             let animspec$(2)=animspec$(2)(1:pos(animspec$(2)," ",-1))&str$(len(trim$(text$)))
26656             print #animwindow, fields mat animspec$ : animation$(animframe), trim$(text$)
26658           else 
26660             print #animwindow, fields mat animspec$ : animation$(animframe)
26662           end if 
26664         end if 
26666       end if 
26668     end if 
26670   fnend 
26672 ! 
26674   def fncloseanim
26676     if animwindow then 
26678       close #animwindow: 
26680       let animwindow=0
26682     end if 
26684   fnend 
26686 ! 
26688   dim animation$(1)*255
26690   def fnreadclockfolder(folder$*255,mat animation$,&animspeed,&vsize,&hsize,&clockcolor$,&clockborder$;___,index,settings)
26692     if exists(folder$) then 
26694       if udim(mat animation$)=1 and animation$(1)="" then 
26696         let fnreadfiles(mat animation$,folder$,"gif",1,0,1)
26698         let fnreadfiles(mat animation$,folder$,"jpg",0,0,1)
26700 ! 
26702         for index=1 to udim(mat animation$)
26704           let animation$(index)=folder$&"\"&animation$(index)&":NORESIZE"
26706         next index
26708 ! 
26710         let fnsortanimations(mat animation$)
26712       end if 
26714       if ~vsize or ~hsize then 
26716 ! .         ! Read Clock File
26718         if exists(folder$&"\clock.ini") then 
26720           open #(settings:=fngetfilenumber): "name="&folder$&"\clock.ini",display,input 
26722           input #settings: animspeed,vsize,hsize,clockcolor$,clockborder$ error ignore
26724           close #settings: 
26726         end if 
26728 ! 
26730         if ~vsize then let vsize=5 ! Default Size
26732         if ~hsize then let hsize=10 ! Default Size
26734       end if 
26736       if udim(mat animation$) then 
26738         let fnreadclockfolder=1
26740       else 
26742         let fnreadclockfolder=-1
26744       end if 
26746     else 
26748       let fnreadclockfolder=-1
26750     end if 
26752   fnend 
26754 ! 
26756   dim srt_animation$(1)*255
26758   dim sort(1)
26760   def fnsortanimations(mat animation$;___,index)
26762     mat srt_animation$(udim(animation$))=animation$
26764     mat sort(udim(animation$))=aidx(animation$)
26766 ! 
26768     for index=1 to udim(mat sort)
26770       let animation$(index)=srt_animation$(sort(index))
26772     next index
26774   fnend 
26776 ! 
26778   dim lastindex
26780   dim lvac_start(1)
26782   dim lvac_end(1)
26784   dim lvac_color$(1)*40
26786 ! 
26788 APPLYCOLOR: ! Function To Apply A Color Array To A Listview
26790   def fnsetcolors(mat color$,mat lvac_start,mat lvac_end,mat lvac_color$;___,index,color$*40,start)
26792     for index=1 to udim(mat color$)
26794       if color$<>color$(index) then 
26796         let color$=color$(index)
26798         if udim(mat lvac_end) and ~lvac_end(udim(mat lvac_end)) then 
26800           let lvac_end(udim(mat lvac_end))=index+lastindex-1
26802         end if 
26804         if color$(1:1)="/" or color$(1:1)="[" then 
26806           mat lvac_end(udim(mat lvac_end)+1)
26808           mat lvac_start(udim(mat lvac_end))
26810           mat lvac_color$(udim(mat lvac_end))
26812 ! 
26814           let lvac_start(udim(mat lvac_start))=index+lastindex
26816           let lvac_color$(udim(mat lvac_color$))=color$
26818         end if 
26820       end if 
26822     next index
26824     let lastindex+=udim(mat color$)
26826     if udim(mat lvac_end) and ~lvac_end(udim(mat lvac_end)) then 
26828       let lvac_end(udim(lvac_end))=udim(mat color$)
26830     end if 
26832   fnend 
26834 ! 
26836   def fnapplycolor(window,spec$)
26838     if udim(mat lvac_end)>0 then 
26840       print #window, fields spec$&",ATTR" : (mat lvac_start, mat lvac_end, mat lvac_color$)
26842     end if 
26844   fnend 
26846 ! 
26848   def fnclearcolorarrays
26850     mat lvac_end(0)
26852     mat lvac_start(0)
26854     mat lvac_color$(0)
26856     let lastindex=0
26858     mat color$(0)
26860   fnend 
26862 ! 
26864 CALCULATELISTVIEWHEADERS: ! Calculate The Columns And Spec For A Listview
26866   def fncalculatelistviewheaders$*255(mat caption$, mat wid, mat spec$,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;active,&count,___,index,attributes$*255)
26868     let count=0
26870     mat caption$(count) : mat wid(count) : mat spec$(count)
26872     for index=1 to udim(mat fieldtype$)
26874       if lwrc$(trim$(fieldtype$(index)))="listchld" and uprc$(trim$(parent$(index)))=uprc$(trim$(parent$(control))) then 
26876         let count+=1
26878         mat caption$(count) : mat wid(count) : mat spec$(count)
26880         let caption$(count)=trim$(description$(index))
26882         let wid(count)=width(index)
26884         let attributes$=attr$(index)
26886         if fnvalidhexcolor(fgcolor$(index)) and fnvalidhexcolor(bgcolor$(index)) then let attributes$=attributes$&fncolor$(0,fieldtype$(index),fgcolor$(index),bgcolor$(index))
26888         if ~fnvalidattribute(attributes$,fieldtype$(index)) then 
26890           let attributes$=""
26892         end if 
26894         let spec$(count)="C"&trim$(justify$(index))&" "&str$(specwidth(index))&","&attributes$
26896 ! .         ! Apply conversion in Header if we should
26898         if fnapplyconversion(cnvrtin$(index),1) then let spec$(count)=cnvrtin$(index)&","&attributes$
26900       end if 
26902     next index
26904     let fncalculatelistviewheaders$=fncalculatelistviewspec$(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,active,1)
26906   fnend 
26908 ! 
26910   def fnapplyconversion(&convert$;header)
26912     if len(trim$(convert$)) then 
26914       if fn42 and trim$(lwrc$(convert$))(1:3)<>"pic" and trim$(lwrc$(convert$))(1:3)<>"fmt" then 
26916 ! .         ! Apply at Header
26918         let fnapplyconversion=header
26920       else 
26922 ! .         ! Apply Manually
26924         let fnapplyconversion=~header
26926       end if 
26928     end if 
26930   fnend 
26932 ! 
26934   dim combo$(1)*255 ! 
26936 POPULATECOMBO: ! This Function Populates All The Combo Boxes On The Screen
26938   def fnpopulatecombo(window,fdatafile,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat form$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;justthisone,ignorefilters,editing,whichone,___,attribute$*128,index,spec$*60,position,select$)
26940     for index=1 to udim(mat fieldtype$)
26942       if whichone>=0 or whichone*-1=index then 
26944         if (index=justthisone) or (not justthisone) then 
26946           if editing or ~invisible(index) then 
26948             if lwrc$(trim$(fieldtype$(index))) = "combo" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "combo"
26950               mat combo$(0)
26952               if ~ignorefilters and len(trim$(function$(index))) then 
26954 ! .                     ! run your populate function, pass it mat combo$
26956                 let fnexecutegetdata(function$(index),mat combo$,index)
26958               end if 
26960               let attribute$="="
26962               let select$=""
26964               if gridlines(index) then let select$=",SELECT"
26966               let spec$=fncalculatespec$(window,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
26968 ! 
26970               let position=pos(spec$,",",-1)
26972               let spec$(position:position-1)=select$ ! insert select attribute
26974 ! 
26976               let position=pos(spec$,",",pos(spec$,",",pos(spec$,",")+1)+1)
26978               let spec$(position+1:position)=attribute$ ! insert populate attribute
26980 ! 
26982               print #window, fields spec$ : mat combo$
26984             else ! #Case Else#
26986 ! .                  ! Ignore Non-Combo Controls
26988             end if  ! #End Select#
26990           end if 
26992         end if 
26994       end if 
26996     next index
26998   fnend 
27000 ! 
27002 CALCULATELISTVIEWSPEC: ! Calculate The Listview Spec Here
27004   def fncalculatelistviewspec$*255(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;active,headers,___,attributes$*128,selected)
27006     if headers then 
27008       let attributes$=attr$(control)
27010       let attributes$=fnremoveprotectedattribute$(attributes$)
27012       if ~fnvalidattribute(attributes$,fieldtype$(control)) then 
27014         let attributes$=""
27016       end if 
27018       if sum(selectedcontrols) then 
27020         mat selectedcontrols(udim(mat controlname$))
27022         let selected=selectedcontrols(control)
27024       end if 
27026       let fncalculatelistviewspec$=str$(vposition(control))&","&str$(hposition(control))&",LIST "&str$(height(control))&"/"&str$(width(control))&",HEADERS"&","&attributes$&fncolor$(active,fieldtype$(control),fgcolor$(control),bgcolor$(control),selected,invisible(control))&","&fnmakeindex$(fieldtype$(control),control)
27028     else 
27030       let fncalculatelistviewspec$=str$(vposition(control))&","&str$(hposition(control))&",LIST "&str$(height(control))&"/"&str$(width(control))
27032     end if 
27034   fnend 
27036 ! 
27038   def fncontroldescription$*255(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;favorcontrolname)
27040     if favorcontrolname then 
27042       if len(trim$(controlname$(control))) then 
27044         let fncontroldescription$=controlname$(control)
27046       else if len(trim$(description$(control))) then 
27048         let fncontroldescription$=description$(control)
27050       else 
27052         let fncontroldescription$=str$(vposition(control))&","&str$(hposition(control))&","&fncalculatefieldtype$(fieldtype$(control),justify$(control),width(control),specwidth(control),height(control))
27054       end if 
27056     else 
27058       if len(trim$(description$(control))) then 
27060         let fncontroldescription$=description$(control)
27062       else if len(trim$(controlname$(control))) then 
27064         let fncontroldescription$=controlname$(control)
27066       else 
27068         let fncontroldescription$=str$(vposition(control))&","&str$(hposition(control))&","&fncalculatefieldtype$(fieldtype$(control),justify$(control),width(control),specwidth(control),height(control))
27070       end if 
27072     end if 
27074   fnend 
27076 ! 
27078 CALCULATEFRAMESPEC: ! Calculate Frame Spec here
27080   def fncalculateframespec$*255(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;active,runtime,___,attributes$*128,selected,text$*255,dummy$*255,color$*255)
27082     if runtime then 
27084       let text$=description$(control)
27086     else if ~active and len(trim$(description$(control))) then 
27088       let text$=description$(control)
27090     else if ~active and len(trim$(controlname$(control))) then 
27092       let text$=controlname$(control)
27094     else 
27096       let text$=str$(vposition(control))&"."&str$(hposition(control))&"."&str$(width(control))&"/"&str$(height(control))
27098     end if 
27100     let attributes$=",N="&attr$(control)
27102     if ~fnvalidframe(attributes$) then let attributes$=""
27104     let color$=fncolor$(active,fieldtype$(control),fgcolor$(control),bgcolor$(control),selected,invisible(control))
27106     if ~len(trim$(color$)) then let color$=""
27108     let attributes$=",N="&color$
27110     if gridlines(control) then 
27112       let attributes$=",border=S"&color$&attributes$
27114       if len(trim$(text$)) then 
27116         let attributes$=attributes$&",caption="&trim$(text$)
27118       end if 
27120     end if 
27122     if len(trim$(picture$(control))) then 
27124       let dummy$=picture$(control)
27126       if pos(dummy$,":",-1)>4 then let dummy$=dummy$(1:pos(dummy$,":")-1)
27128       if exists(dummy$) then 
27130         let attributes$=attributes$&",picture="&picture$(control)
27132       end if 
27134     end if 
27136 ! 
27138     let fncalculateframespec$="srow="&str$(vposition(control))&",scol="&str$(hposition(control))&",rows="&str$(height(control))&",cols="&str$(width(control))&attributes$
27140   fnend 
27142 ! 
27144 CALCULATESPEC: !  Calculate Spec For Editor Control
27146   def fncalculatespec$*255(window,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;active,editing,otherchanged,___,target$*32,target,attributes$*383,alternatespec$*255,finalattr$*383,protect,framespec$*255,posofcomma)
27148     if lwrc$(trim$(fieldtype$(index)))="search" or lwrc$(trim$(fieldtype$(index)))="filter" then 
27150       let target=fnfindtarget(parent$(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
27152       if target>0 then 
27154         let target$=","&str$(vposition(target))&","&str$(hposition(target))
27156       else 
27158         let target$=",1,1" ! Just Has To Be Something Valid
27160       end if 
27162       if lwrc$(trim$(fieldtype$(index)))="filter" then 
27164         let fncleanfilter(picture$(index))
27166 ! 
27168         let target$=target$&","&trim$(picture$(index),",")
27170       end if 
27172     end if 
27174     let attributes$=attr$(index)
27176     if protected(index) then 
27178       if lwrc$(trim$(fieldtype$(index))) = "c" then ! #Select# lwrc$(trim$(FieldType$(Index))) #Case# "c"
27180         if len(trim$(screenio$(si_protectedtext))) then 
27182           let attributes$=trim$(attributes$)&trim$(screenio$(si_protectedtext))
27184           let protect=1
27186         end if 
27188       else if lwrc$(trim$(fieldtype$(index))) = "button" then ! #Case# "button"
27190         if len(trim$(screenio$(si_protectedbutton))) then 
27192           let attributes$=trim$(attributes$)&trim$(screenio$(si_protectedbutton))
27194           let protect=1
27196         end if 
27198       else if lwrc$(trim$(fieldtype$(index))) = "check" then ! #Case# "check"
27200         if len(trim$(screenio$(si_protectedcheck))) then 
27202           let attributes$=trim$(attributes$)&trim$(screenio$(si_protectedcheck))
27204           let protect=1
27206         end if 
27208       else if lwrc$(trim$(fieldtype$(index))) = "search" or lwrc$(trim$(fieldtype$(index))) = "filter" then ! #Case# "search" # "filter"
27210         if len(trim$(screenio$(si_protectedtext))) then 
27212           let attributes$=trim$(attributes$)&trim$(screenio$(si_protectedtext))
27214           let protect=1
27216         end if 
27218       end if  ! #End Select#
27220     end if 
27222     if editing then 
27224       let attributes$=fnremoveprotectedattribute$(attributes$)
27226       if ~fnvalidattribute(attributes$,fieldtype$(index)) then 
27228         let attributes$=""
27230       end if 
27232       mat selectedcontrols(udim(mat fieldtype$))
27234     else 
27236       if uprc$(cnvrtin$(index))(1:4)="FMT(" then let alternatespec$=cnvrtin$(index)
27238     end if 
27240     if fn42ia and fnmultilinetextbox(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
27242       if fn42jd then 
27244         let attributes$=attributes$&"^ENTER_CRLF"
27246       else 
27248         let attributes$=attributes$&"^ENTER-CRLF"
27250       end if 
27252     end if 
27254     let finalattr$=attributes$&fncolor$(active,fieldtype$(index),fgcolor$(index),bgcolor$(index),(editing and selectedcontrols(index)),(editing and invisible(index)),protect,otherchanged)&target$
27256     let fncalculatespec$=str$(vposition(index))&","&str$(hposition(index))&","&fncalculatefieldtype$(fieldtype$(index),justify$(index),width(index),specwidth(index),height(index),attr$(index),alternatespec$)&","&finalattr$&","&fnmakeindex$(fieldtype$(index),index)
27258 ! 
27260     if window then 
27262 ! .      ! if its a multiline textbox, button, or caption then
27264 ! .      ! open a frame tied to the thing by unique Parent$ value
27266       if lwrc$(trim$(fieldtype$(index))) = "c" or lwrc$(trim$(fieldtype$(index))) = "button" or lwrc$(trim$(fieldtype$(index))) = "caption" then ! #Select# lwrc$(trim$(fieldtype$(Index))) #Case# "c" # "button" # "caption"
27268 ! .         ! We need to test if frame is broken first before reprinting it. We only need to reprint if it moves or changes
27270         let framespec$=fncalculateframespec$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
27272         let fncloseframewindow(parent$(index),framespec$)
27274         if height(index)>1 then 
27276           let fnchangeforcevisibility(0)
27278           let fnopenframewindow(window,framespec$,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
27280           let fnchangeforcevisibility(1)
27282         end if 
27284       end if  ! #End Select#
27286     end if 
27288   fnend 
27290 ! 
27292 ! 
27294 CALCULATEDATA: !  Calculate Data For Editor Control
27296   def fncalculatedata$*255(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;active,runtime,___,data$*255,target,target$,dummy$*255)
27298     if lwrc$(trim$(fieldtype$(index))) = "caption" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index)))   #Case# "caption"
27300       if runtime or (~active and len(trim$(description$(index)))) then 
27302         let data$=description$(index)
27304 !            If Data$="" Then Let Data$=" "
27306       else if ~active and len(trim$(controlname$(index))) then 
27308         let data$=controlname$(index)
27310       else 
27312         let data$=str$(vposition(index))&","&str$(hposition(index))&","&fncalculatefieldtype$(fieldtype$(index),justify$(index),width(index),specwidth(index),height(index))
27314       end if 
27316       let fnchangespecwidth(index,len(data$))
27318     else if lwrc$(trim$(fieldtype$(index))) = "check" then ! #Case# "check"
27320       if runtime or (~active and len(trim$(description$(index)))) then 
27322         let data$=description$(index)
27324         if data$="" then let data$=" "
27326       else if ~active and len(trim$(controlname$(index))) then 
27328         let data$=controlname$(index)
27330       else if active then 
27332         let data$=str$(vposition(index))&","&str$(hposition(index))&","&fncalculatefieldtype$(fieldtype$(index),justify$(index),width(index),specwidth(index),height(index),attr$(index))
27334       else 
27336         let data$=""
27338       end if 
27340 ! 
27342       if active then let data$="^"&data$
27344       let fnchangespecwidth(index,len(data$)+1)
27346     else if lwrc$(trim$(fieldtype$(index))) = "button" then ! #Case# "button"
27348       if runtime or (~active and len(trim$(description$(index)))) then 
27350         let data$=description$(index)
27352         if data$="" then let data$=" "
27354       else if ~active and len(trim$(function$(index))) then 
27356         let data$=function$(index)
27358       else if ~active and len(trim$(controlname$(index))) then 
27360         let data$=controlname$(index)
27362       else 
27364         let data$=str$(vposition(index))&","&str$(hposition(index))&","&fncalculatefieldtype$(fieldtype$(index),justify$(index),width(index),specwidth(index),height(index),"",altspec$)
27366       end if 
27368       let fnchangespecwidth(index,len(data$))
27370     else if lwrc$(trim$(fieldtype$(index))) = "p" then ! #Case# "p"
27372       if runtime or len(trim$(picture$(index))) then 
27374         let data$=picture$(index)
27376         if data$="" then let data$=setting_imagepath$&"\nothing.gif"
27378         if fn42 then 
27380           let dummy$=picture$(index)
27382           if pos(picture$(index),":",-1)>4 then let dummy$=dummy$(1:pos(dummy$,":")-1)
27384           if ~exists(dummy$) then let data$=setting_imagepath$&"\nothing.gif"
27386         end if 
27388       else 
27390         if fn42 then 
27392           let data$=setting_imagepath$&"\yellow.gif"
27394         else 
27396           let data$="nothing.bmp" ! It Doesn't have to exist in 4.1
27398         end if 
27400       end if 
27402     else if lwrc$(trim$(fieldtype$(index))) = "c" then ! #Case# "c"
27404       if runtime then 
27406         let data$=description$(index)
27408         if data$="" then let data$=" "
27410       else if ~active and len(trim$(fieldname$(index))) then 
27412         let data$=fieldname$(index)
27414       else if ~active and len(trim$(controlname$(index))) then 
27416         let data$=controlname$(index)
27418       else 
27420         if uprc$(cnvrtin$(index))(1:4)="FMT(" then let dummy$=cnvrtin$(index) else let dummy$=""
27422         let data$=str$(vposition(index))&","&str$(hposition(index))&","&fncalculatefieldtype$(fieldtype$(index),justify$(index),width(index),specwidth(index),height(index),"",dummy$)
27424       end if 
27426 ! 
27428       let data$=data$(1:specwidth(index))
27430 ! 
27432     else if lwrc$(trim$(fieldtype$(index))) = "search" then ! #Case# "search"
27434       let target=fnfindtarget(parent$(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
27436       if target>0 then 
27438         let target$=",,"&str$(vposition(target))&","&str$(hposition(target))
27440       end if 
27442 ! 
27444       if runtime then 
27446         let data$=description$(index)
27448         if data$="" then let data$=" "
27450       else if ~active and len(trim$(controlname$(index))) then 
27452         let data$=controlname$(index)
27454       else 
27456         let data$=str$(vposition(index))&","&str$(hposition(index))&","&fncalculatefieldtype$(fieldtype$(index),justify$(index),width(index),specwidth(index),height(index))&target$
27458       end if 
27460 ! 
27462       let data$=data$(1:specwidth(index))
27464 ! 
27466     else if lwrc$(trim$(fieldtype$(index))) = "filter" then ! #Case# "filter"
27468       let target=fnfindtarget(parent$(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
27470       if target>0 then 
27472         let target$=",,"&str$(vposition(target))&","&str$(hposition(target))
27474       end if 
27476 ! 
27478       if runtime then 
27480         let data$=description$(index)
27482         if data$="" then let data$=" "
27484       else if ~active and len(trim$(controlname$(index))) then 
27486         let data$=controlname$(index)
27488       else 
27490         let data$=str$(vposition(index))&","&str$(hposition(index))&","&fncalculatefieldtype$(fieldtype$(index),justify$(index),width(index),specwidth(index),height(index))&target$&","&picture$(index)
27492       end if 
27494 ! 
27496       let data$=data$(1:specwidth(index))
27498 ! 
27500     else if lwrc$(trim$(fieldtype$(index))) = "listchld" then ! #Case# "listchld"
27502     else if lwrc$(trim$(fieldtype$(index))) = "listview" then ! #Case# "listview"
27504     else if lwrc$(trim$(fieldtype$(index))) = "screen" then ! #Case# "screen"
27506     else if lwrc$(trim$(fieldtype$(index))) = "frame" then ! #Case# "frame"
27508     else ! #Case Else#
27510       if runtime then 
27512         let data$=description$(index)
27514         if data$="" then let data$=" "
27516       else if ~active and len(trim$(fieldname$(index))) then 
27518         let data$=fieldname$(index)
27520       else if ~active and len(trim$(controlname$(index))) then 
27522         let data$=controlname$(index)
27524       else 
27526         let data$=str$(vposition(index))&","&str$(hposition(index))&","&fncalculatefieldtype$(fieldtype$(index),justify$(index),width(index),specwidth(index),height(index))
27528       end if 
27530 ! 
27532       if specwidth(index)>0 then 
27534         let data$=data$(1:specwidth(index))
27536       else 
27538         let data$=data$(1:width(index))
27540       end if 
27542     end if  ! #End Select#
27544     let fncalculatedata$=data$
27546   fnend 
27548 ! 
27550 CALCULATEHELP: ! Calculate The Helptext By Adding The "3;" To The Beginning If Necessary.
27552   def fncalculatehelp$*260(helptext$*260;___,x,helplevel$)
27554     let fnfixtooltips(helptext$)
27556     let fncalculatehelp$=helptext$
27558   fnend 
27560 ! 
27562   def library fnbr42=fn42
27564   def library fnbr43=fn43
27566   def fn42=(val(wbversion$(1:3))>=4.2)
27568   def fn43=(val(wbversion$(1:3))>=4.3)
27570   def fn42ia=((wbversion$=="4.20ia") or (fn42 and (wbversion$(5:5)>"i"))) or fn43
27572   def fn42jd=(wbversion$>="4.20jd") or fn43
27574   def fn42e=(wbversion$>="4.20e") or fn43
27576 ! 
27578 ! 
27580 CALCULATEFIELDTYPE: ! Caluclate Display Spec Based On Field Type
27582   def fncalculatefieldtype$*255(type$,justify$,width,specwidth,height;attr$*128,altspec$*255)
27584     if lwrc$(trim$(type$)) = "c" then ! #Select# Lwrc$(Trim$(Type$)) #Case# "c"
27586 !         if Height>1 then let Width=Width+((Height-1)*Screenio(Si_Hsize))
27588       let width=width*max(height,1)
27590       if altspec$="" then 
27592         let fncalculatefieldtype$=str$(width)&"/"&fndisplayfieldtype$(type$)&trim$(justify$)&" "&str$(specwidth)
27594       else 
27596         let fncalculatefieldtype$=str$(width)&"/"&altspec$
27598       end if 
27600     else if lwrc$(trim$(type$)) = "combo" then ! #Case# "combo"
27602       let width=width*max(height,1)
27604       if altspec$="" then 
27606         let fncalculatefieldtype$=str$(width)&"/"&fndisplayfieldtype$(type$)&" "&str$(specwidth)
27608       else 
27610         let fncalculatefieldtype$=str$(width)&"/"&altspec$
27612       end if 
27614     else if lwrc$(trim$(type$)) = "p" then ! #Case# "p"
27616       let fncalculatefieldtype$=fndisplayfieldtype$(type$)&" "&str$(height)&"/"&str$(width)
27618     else if lwrc$(trim$(type$)) = "button" then ! #Case# "button"
27620       let width=width*max(height,1)
27622       let fncalculatefieldtype$=str$(width)&"/"&fndisplayfieldtype$(type$)&" "&str$(max(specwidth,1))
27624     else if lwrc$(trim$(type$)) = "caption" then ! #Case# "caption"
27626 !         if Height>1 then let Width=Width+((Height-1)*Screenio(Si_Hsize))
27628       let width=width*max(height,1)
27630       let fncalculatefieldtype$=str$(width)&"/"&fndisplayfieldtype$(type$)&trim$(justify$)&" "&str$(specwidth)
27632     else if lwrc$(trim$(type$)) = "listchld" then ! #Case# "listchld"
27634     else if lwrc$(trim$(type$)) = "listview" then ! #Case# "listview"
27636     else if lwrc$(trim$(type$)) = "frame" then ! #Case# "frame"
27638     else if lwrc$(trim$(type$)) = "check" then ! #Case# "check"
27640       if pos("0123456789",attr$(1:1)) then let type$="radio"
27642       let fncalculatefieldtype$=str$(width)&"/"&fndisplayfieldtype$(type$)&" "&str$(max(specwidth,1))
27644     else if lwrc$(trim$(type$)) = "search" or lwrc$(trim$(type$)) = "filter" then ! #Case# "search" # "filter"
27646       let fncalculatefieldtype$=str$(width)&"/"&fndisplayfieldtype$(type$)&" "&str$(specwidth)
27648     else ! #Case Else#
27650       let fncalculatefieldtype$=fndisplayfieldtype$(type$)&" "&str$(width)
27652     end if  ! #End Select#
27654   fnend 
27656 ! 
27658 DISPLAYFIELDTYPE: !  Calculate Display Field Type
27660   def fndisplayfieldtype$(type$)
27662     if lwrc$(trim$(type$)) = "caption" then ! #Select# Lwrc$(Trim$(Type$)) #Case# "caption"
27664       let fndisplayfieldtype$="C"
27666     else if lwrc$(trim$(type$)) = "button" then ! #Case# "button"
27668       let fndisplayfieldtype$="CC"
27670     else if lwrc$(trim$(type$)) = "listview" then ! #Case# "listview"
27672     else if lwrc$(trim$(type$)) = "listchld" then ! #Case# "listchld"
27674     else ! #Case Else#
27676       let fndisplayfieldtype$=type$
27678     end if  ! #End Select#
27680   fnend 
27682 COLOR: ! Determine Color Spec Based On Type
27684   def fncolor$*255(active,type$,fgcolor$,bgcolor$;selected,invisible,protected,otherchanged,___,sunken$,color$*255)
27686     if otherchanged then 
27688       if len(trim$(screenio$(si_otherchanges))) then 
27690         let color$=trim$(screenio$(si_otherchanges))
27692       else 
27694         let color$="/#000000:#FF99FF" ! Pink
27696       end if 
27698     else 
27700       if active then 
27702         if selected then 
27704           let bgcolor$="A0FF00" ! Yellow Green
27706         else 
27708           if invisible then 
27710             let bgcolor$="FF9933" ! Orange (Yellow Pink)
27712           else 
27714             let bgcolor$="FFFF00" ! Yellow
27716           end if 
27718         end if 
27720       else 
27722         if selected then 
27724           let bgcolor$="00FF00" ! Green
27726         else if invisible then 
27728           let bgcolor$="FF99FF" ! Pink
27730         end if 
27732       end if 
27734       if ~protected and (len(trim$(fgcolor$)) or len(trim$(bgcolor$))) then 
27736         if ~fnvalidhexcolor(fgcolor$) then let fgcolor$="W"
27738         if ~fnvalidhexcolor(bgcolor$) then let bgcolor$="W"
27740         if len(trim$(fgcolor$))=6 then let fgcolor$="#"&fgcolor$
27742         if len(trim$(bgcolor$))=6 then let bgcolor$="#"&bgcolor$
27744         let fgcolor$=trim$(fgcolor$)
27746         let bgcolor$=trim$(bgcolor$)
27748         let color$="/"&fgcolor$&":"&bgcolor$
27750       end if 
27752 ! .! .!
27754       if lwrc$(trim$(type$)) = "c" then ! #Select# Lwrc$(Trim$(Type$)) #Case# "c"
27756         let sunken$="S"
27758       else ! #Case Else#
27760         let sunken$=""
27762       end if  ! #End Select#
27764     end if 
27766     let fncolor$=sunken$&color$
27768   fnend 
27770 MAKEINDEX: !  Calculate Index Based On Field Type   (Button Or Otherwise)
27772   def fnmakeindex$(type$,index)
27774     if lwrc$(trim$(type$)) = "button" then ! #Select# Lwrc$(Trim$(Type$)) #Case#  "button"
27776       let fnmakeindex$="B"&str$(fnkeybase+index)
27778     else ! #Case Else#
27780       let fnmakeindex$=str$(fnkeybase+index)
27782     end if  ! #End Select#
27784   fnend 
27786 ! 
27788 REMOVEPROTECTEDATTRIBUTE: ! Strip Protected Attribute
27790   def fnremoveprotectedattribute$*128(wrkattr$*128;___,ploop,spot)
27792     for ploop = 1 to len(wrkattr$)
27794       if (spot:=pos(wrkattr$,"[",ploop))>0 then 
27796         let wrkattr$(ploop:spot-1)=srep$(wrkattr$(ploop:spot-1),"p","")
27798         let wrkattr$(ploop:spot-1)=srep$(wrkattr$(ploop:spot-1),"P","")
27800         let ploop = pos(wrkattr$,"]",spot)
27802         if ploop<1 then 
27804           let ploop=len(wrkattr$)+5
27806         end if 
27808       else 
27810         let wrkattr$(ploop:len(wrkattr$)) = srep$(wrkattr$(ploop:len(wrkattr$)),"p","")
27812         let wrkattr$(ploop:len(wrkattr$)) = srep$(wrkattr$(ploop:len(wrkattr$)),"P","")
27814         let ploop=len(wrkattr$)+5
27816       end if 
27818     next ploop
27820     let fnremoveprotectedattribute$=wrkattr$
27822   fnend 
27824 ! 
27826 ATTRIBUTE: ! Check For A Specific Attribute
27828   def fnattribute(searchfor$,listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,parent$,control,startp,endp,attribute$*255,broken) ! Returns True If Theres An L Attribute
27830     if listviewindex and listviewindex<=udim(mat parent$) then 
27832       let parent$=parent$(listviewindex)
27834       for control=1 to udim(mat parent$)
27836         if parent$(control)=parent$ then 
27838           let broken=0
27840           let attribute$=attr$(control)
27842           do while (startp:=pos(attribute$,"["))>0
27844             if (endp:=pos(attribute$,"]",startp)) then 
27846               let attribute$(startp:endp)=""
27848             else 
27850               let broken=1
27852             end if 
27854           loop until broken
27856           if ~broken then 
27858             if pos(lwrc$(attribute$),lwrc$(searchfor$))>0 then 
27860               let fnattribute=1
27862               let control=udim(mat parent$)
27864             end if 
27866           end if 
27868         end if 
27870       next control
27872     end if 
27874   fnend 
27876 ! 
27878 ! *****************************************************************
27880 ! ********************** Input Listview Fields ********************
27882 ! *****************************************************************
27884 INPUTLISTVIEWFIELDS: ! Create A Grid Containing All Listview Columns
27886   def fninputlistviewfields(&mode, &control, mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,window,function,weditor,spec$*60,index,columns,key$,lastchild,wfields,inputrow,currentcolumn,direction,fromindex,toindex,fkeyvalue$,wgrid,lspec$*64,erow,ecol,lwidth,rows,cols)
27888 ! 
27890     if control>0 and lwrc$(trim$(fieldtype$(control)))="listview" then 
27892       let weditor=fngeteditorwindow
27894 ! 
27896 ! .       ! Open Window to the Left with the row captions
27898       let window=fnopengridheadingswindow(13,15,vposition(control),hposition(control))
27900       let fnprintgridheadingswindow(window,15)
27902 ! 
27904       if fn43 then 
27906         execute "config keyboard C000 1300" ! Ctrl <- To Shift F9  (Fkey 19)
27908         execute "config keyboard C100 1500" ! Ctrl -> To Shift F10 (Fkey 21)
27910       else 
27912         execute "config keyboard 7600 1300" ! Ctrl <- To Shift F9  (Fkey 19)
27914         execute "config keyboard 7500 1500" ! Ctrl -> To Shift F10 (Fkey 21)
27916       end if 
27918       execute "config keyboard 0B00 0A0A0A636F6E206B657920636C6561720D" ! Shift F1 Key To "con key clear<CR>"
27920 ! 
27922       let fngeteditorposition(erow,ecol)
27924       let lspec$=fncalculatelistviewheaders$(mat draw_lv_caption$, mat draw_lv_width, mat draw_lv_spec$,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,active,columns)
27926 ! 
27928       let erow=vposition(control)
27930       let ecol=hposition(control)
27932       let fncalculaterealposition(erow,ecol)
27934       let lwidth=width(control)
27936       let fngetscreensize(rows,cols)
27938       let erow=min(rows-13,erow)
27940 ! 
27942 ! .      ! Open it at position of editor window + position of listview, width of listview, height of 10
27944       open #(wgrid:=fngetfilenumber): "srow="&str$(erow)&",scol="&str$(ecol)&",rows=12,cols="&str$(lwidth),display,outin 
27946 ! 
27948       do 
27950 ! .          ! Draw Listview Header
27952         let spec$=fncalculatelistviewheaders$(mat draw_lv_caption$, mat draw_lv_width, mat draw_lv_spec$,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,active,columns)
27954         let spec$=srep$(uprc$(spec$),"LIST","GRID")
27956         let spec$(1:pos(spec$,"/")-1)="1,1,GRID 12"
27958 ! 
27960         if fn42 then 
27962           mat draw_lv_spec$=("C 255,^nosort") ! Reset All Specs To Attribute Grid Specs
27964         else 
27966           mat draw_lv_spec$=("C 255") ! Reset All Specs To Attribute Grid Specs
27968         end if 
27970         for index=1 to udim(mat draw_lv_width)
27972           let draw_lv_width(index)=max(1,draw_lv_width(index))
27974         next index
27976 ! 
27978         print #wgrid, fields spec$ : (mat draw_lv_caption$, mat draw_lv_width, mat draw_lv_spec$)
27980 ! 
27982 ! .         ! Populate Listview with Column Data
27984         let fnloadlistchlddata(control,columns,mat draw_lv_data$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
27986 ! 
27988         let spec$=fncalculatelistviewspec$(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
27990         let spec$=srep$(uprc$(spec$),"LIST","GRID")
27992         let spec$(1:pos(spec$,"/")-1)="1,1,GRID 12"
27994         let fkeyvalue$=","&fnmakeindex$(fieldtype$(control),control)
27996         print #wgrid, fields spec$&",="&fkeyvalue$ : (mat draw_lv_data$)
27998 ! 
28000 ! .         ! Next Input from Grid
28002         if udim(mat draw_lv_data$) and sum(mat draw_lv_width) then 
28004           input #wgrid, fields spec$&",CELL,ALL"&fkeyvalue$ : (mat draw_lv_data$)
28006           let currentcolumn=curcol
28008         else 
28010           input #0, fields "1,2,C 1,AEX" : key$
28012           let currentcolumn=0
28014         end if 
28016         let function=fkey
28018 ! 
28020 ! .         ! Store Results Back into Child Arrays
28022         let fnsavelistchlddata(control,columns,mat draw_lv_data$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28024 ! 
28026         if function=0 then 
28028           let function=1500+control
28030         end if 
28032 ! 
28034         if function=19 or function=21 then ! Ctrl Left Arrow (Shift Column)
28036           let direction=function-20 ! -1 For Left, 1 For Right
28038           if currentcolumn>0 and currentcolumn<=columns and currentcolumn+direction>0 and currentcolumn+direction<=columns then 
28040             let fromindex=fnfindlistviewchild(currentcolumn,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28042             let toindex=fnfindlistviewchild(currentcolumn+direction,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28044             let fnswapcontrolarrays(fromindex,toindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28046             let curfld(1,(columns*(currow-1))+currentcolumn+direction)
28048           else 
28050             print bell
28052             let curfld(1,(columns*(currow-1))+currentcolumn)
28054           end if 
28056         end if 
28058         if function=1000 then ! Add Column
28060           let fnaddlistviewchild(parent$(control),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28062         end if 
28064         if function=1001 then ! Delete Column
28066 ! .            ! Find Current Column. Delete That One. Shift All Others.
28068           let fromindex=fnfindlistviewchild(currentcolumn,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28070 ! 
28072           for index=fromindex to udim(mat controlname$)-1
28074             let fncopycontrolarrays(index,index+1,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28076           next index
28078           let fnresizecontrolarrays(udim(mat controlname$)-1,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28080         end if 
28082         if function=1300 then 
28084           let wfields=fngetfieldswindow
28086           let fncolorfieldsactive(1)
28088           do 
28090             let fngetfieldsspec(spec$)
28092             rinput #wfields, fields spec$ : inputrow
28094             let function=fkey
28096             if function=1300 or function=0 or function=201 then 
28098               let fnaddcurrentlistchld(parent$(control),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28100               let function=0
28102 ! 
28104               let spec$=fncalculatelistviewheaders$(mat draw_lv_caption$, mat draw_lv_width, mat draw_lv_spec$,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,active,columns)
28106               let spec$=srep$(uprc$(spec$),"LIST","GRID")
28108               let spec$(1:pos(spec$,"/")-1)="1,1,GRID 12"
28110 ! 
28112               mat draw_lv_spec$=("C 255") ! Reset All Specs To Attribute Grid Specs
28114               for index=1 to udim(mat draw_lv_width)
28116                 let draw_lv_width(index)=max(1,draw_lv_width(index))
28118               next index
28120               print #wgrid, fields spec$ : (mat draw_lv_caption$, mat draw_lv_width, mat draw_lv_spec$)
28122               let fnloadlistchlddata(control,columns,mat draw_lv_data$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28124               let spec$=fncalculatelistviewspec$(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
28126               let spec$=srep$(uprc$(spec$),"LIST","GRID")
28128               let spec$(1:pos(spec$,"/")-1)="1,1,GRID 12"
28130 ! 
28132               let fkeyvalue$=","&fnmakeindex$(fieldtype$(control),control)
28134               print #wgrid, fields spec$&",="&fkeyvalue$ : (mat draw_lv_data$)
28136             end if 
28138           loop while function=0
28140           if function=1500+control then 
28142             let function=0
28144           end if 
28146           let fncolorfieldsactive(0)
28148         end if 
28150       loop until function>1100 or function=99 or function=98 or function=93 or function=44
28152       close #window: ! Close Gridheadings Window
28154       close #wgrid: ! Close Grid editing window
28156       execute "config keyboard clear"
28158       let fninputlistviewfields=function
28160     else 
28162       let mode=0 ! Reset Mode For Invalid Control
28164     end if 
28166   fnend 
28168 ! 
28170 FINDLISTVIEWCHILD: ! Finds The "Nth" Listview Child
28172   def fnfindlistviewchild(n,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,count)
28174     do while (index:=index+1)<= udim(mat fieldtype$)
28176       if lwrc$(trim$(fieldtype$(index)))="listchld" and parent$(index)=parent$(control) then 
28178         let count+=1
28180       end if 
28182     loop until count=n
28184     let fnfindlistviewchild=index
28186   fnend 
28188 ! 
28190 COUNTCOLUMNS: ! Count The Columns Of A Listview Control
28192   def fncountcolumns(parent$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,count)
28194     for index=1 to udim(mat fieldtype$)
28196       if lwrc$(trim$(fieldtype$(index)))="listchld" and parent$(index)=parent$ then 
28198         let count+=1
28200       end if 
28202     next index
28204     let fncountcolumns=count
28206   fnend 
28208 ! 
28210 OPENGRIDHEADINGSWINDOW: !  Open The Attributes Window In A Place It Wont Be Off The Screen
28212   def fnopengridheadingswindow(rowsize,colsize,row,col;___,window,rows,cols)
28214     let fncalculaterealposition(row, col)
28216     let row=max(2,row) !  Try To The Left
28218     let col=max(2,col-colsize-1)
28220     let fngetscreensize(rows,cols)
28222     let row=min(rows-rowsize,row)
28224     open #(window:=fngetfilenumber) : "SROW="&str$(row)&",SCOL="&str$(col)&",ROWS="&str$(rowsize)&",COLS="&str$(colsize)&",Border=S",display,outin 
28226     let fnopengridheadingswindow=window
28228   fnend 
28230 ! 
28232 PRINTGRIDHEADINGSWINDOW: ! Print The Grid Headings To The Given Window
28234   def fnprintgridheadingswindow(window,width)
28236     dim gh_caption$(1),gh_width(1),gh_spec$(1)
28238     dim gh_data$(10)
28240 ! 
28242     let gh_caption$(1)=""
28244     let gh_width(1)=width
28246     if fn42 then 
28248       let gh_spec$(1)="CR 18,^nosort"
28250     else 
28252       let gh_spec$(1)="CR 18"
28254     end if 
28256 ! 
28258     dim lv_buttons$(2)
28260     dim lv_buttonsspec$(2)*32
28262 ! 
28264     let lv_buttonsspec$(1)="12,1,CC 14,/W:W,B1000"
28266     let lv_buttonsspec$(2)="13,1,CC 14,/W:W,B1001"
28268 ! 
28270     let lv_buttons$(1)="Add Column"
28272     let lv_buttons$(2)="Delete Column"
28274 ! 
28276     let gh_data$(1)="Control Name-"
28278     let gh_data$(2)="Field Name-"
28280     let gh_data$(3)="Caption-"
28282     let gh_data$(4)="Data Width-"
28284     let gh_data$(5)="Display Width-"
28286     let gh_data$(6)="Justify-"
28288     let gh_data$(7)="FgColor-"
28290     let gh_data$(8)="BgColor-"
28292     let gh_data$(9)="Attributes-"
28294     let gh_data$(10)="Column Spec-"
28296 ! 
28298     print #window, fields "1,1,GRID 11/16,HEADERS,/W:W" : (mat gh_caption$, mat gh_width, mat gh_spec$)
28300     print #window, fields "1,1,GRID 11/16,=" : (mat gh_data$)
28302     print #window, fields mat lv_buttonsspec$ : mat lv_buttons$
28304   fnend 
28306 ! 
28308 SAVELISTCHLDDATA: ! Saves The Listview Child Data Back To The Screencontrols Objects
28310   def fnsavelistchlddata(control,columns,mat data$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,dummy$)
28312     for index=1 to udim(data$)
28314       if int((index-1)/columns)+1 = 1 then ! #Select# Int((Index-1)/Columns)+1 #Case# 1
28316         let controlname$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))=trim$(data$(index))(1:20)
28318       else if int((index-1)/columns)+1 = 2 then ! #Case# 2
28320         let fieldname$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))=trim$(data$(index))(1:30)
28322       else if int((index-1)/columns)+1 = 3 then ! #Case# 3
28324         let description$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))=trim$(data$(index))(1:60)
28326       else if int((index-1)/columns)+1 = 4 then ! #Case# 4
28328         let specwidth(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))=val(data$(index)) conv ignore
28330       else if int((index-1)/columns)+1 = 5 then ! #Case# 5
28332         let width(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))=val(data$(index)) conv ignore
28334       else if int((index-1)/columns)+1 = 6 then ! #Case# 6
28336         let dummy$=trim$(uprc$(data$(index)))(len(trim$(data$(index))):len(trim$(data$(index))))
28338         if dummy$="C" or dummy$="U" or dummy$="R" or dummy$="L" or dummy$="" then 
28340           let justify$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))=dummy$
28342         end if 
28344       else if int((index-1)/columns)+1 = 7 then ! #Case# 7
28346         let fgcolor$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))=trim$(data$(index))(1:6)
28348       else if int((index-1)/columns)+1 = 8 then ! #Case# 8
28350         let bgcolor$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))=trim$(data$(index))(1:6)
28352       else if int((index-1)/columns)+1 = 9 then ! #Case# 9
28354         let attr$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))=trim$(data$(index))
28356       else if int((index-1)/columns)+1 = 10 then ! #Case# 10
28358         let cnvrtin$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))=trim$(data$(index))
28360       end if  ! #End Select#
28362     next index
28364   fnend 
28366 ! 
28368 LOADLISTCHLDDATA: ! Loads The Listview Child Data From The Screencontrols Objects
28370   def fnloadlistchlddata(control,columns,mat data$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
28372     mat data$(10*columns)
28374     for index=1 to udim(data$)
28376       if int((index-1)/columns)+1 = 1 then ! #Select# Int((Index-1)/Columns)+1 #Case# 1
28378         let data$(index)=controlname$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
28380       else if int((index-1)/columns)+1 = 2 then ! #Case# 2
28382         let data$(index)=fieldname$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
28384       else if int((index-1)/columns)+1 = 3 then ! #Case# 3
28386         let data$(index)=description$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
28388       else if int((index-1)/columns)+1 = 4 then ! #Case# 4
28390         let data$(index)=str$(specwidth(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)))
28392       else if int((index-1)/columns)+1 = 5 then ! #Case# 5
28394         let data$(index)=str$(width(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)))
28396       else if int((index-1)/columns)+1 = 6 then ! #Case# 6
28398         let data$(index)=justify$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
28400       else if int((index-1)/columns)+1 = 7 then ! #Case# 7
28402         let data$(index)=fgcolor$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
28404       else if int((index-1)/columns)+1 = 8 then ! #Case# 8
28406         let data$(index)=bgcolor$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
28408       else if int((index-1)/columns)+1 = 9 then ! #Case# 9
28410         let data$(index)=attr$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
28412       else if int((index-1)/columns)+1 = 10 then ! #Case# 10
28414         let data$(index)=cnvrtin$(fnfindlistviewchild(mod(index-1,columns)+1,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
28416       end if  ! #End Select#
28418     next index
28420   fnend 
28422 ! 
33000 !  #Autonumber# 33000,2
33002 ! *****************************************************************
33004 ! *********************** Input Control Attr **********************
33006 ! *****************************************************************
33008 PREFORMEDITORINPUT: ! 
33010   def fnpreformeditorinput(&mode, &control, mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,window,function,field,weditor,data$*255,spec$*60,fgcolorindex,bgcolorindex,justifysub,functioneditsub,listviewindex,specwidthindex,temp_fgcolor$,temp_bgcolor$,csopenstring$,screen$,bordersize,screenattr$*255)
33012     dim static_lastcontrol
33014     dim static_curfld
33016     dim oas_spec$(1)*60
33018     dim oas_data$(1)*255
33020     dim oas_captionspec$(1)
33022     dim oas_captiondata$(1)*30
33024     dim oas_tooltip$(1)*255
33026     dim oas_inputspec$(1)*60
33028     dim oas_inputdata$(1)*255
33030     dim oas_inputtooltip$(1)*255
33032     dim oas_subs(1)
33034     dim oas_buttons(1)
33036     if control>0 then 
33038       let weditor=fngeteditorwindow
33040       let fngenerateoaspecs(fieldtype$(control) ,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33042       if udim(mat oas_spec$) then 
33044         let window=fnopenobjectattributeswindow(control,max(udim(mat oas_spec$),udim(mat oas_captionspec$)),32,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
33046         print #window, fields mat oas_captionspec$ : mat oas_captiondata$
33048         let fnobjecttoscreen(control,mat oas_subs,mat oas_data$,mat oas_spec$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
33050 ! 
33052         if (justifysub:=srch(mat oas_subs,sf_justify)) > 0 then 
33054           let fnpopulatejustify(oas_spec$(justifysub),window)
33056         end if 
33058         if (functioneditsub:=srch(mat oas_subs,sf_function))>0 then 
33060           print #window, fields fnfunctioneditspec$(oas_spec$(functioneditsub),functioneditsub) : "Edit"
33062         end if 
33064         if (functioneditsub:=srch(mat oas_subs,sf_cnvrtin))>0 then 
33066           print #window, fields fnfunctioneditspec$(oas_spec$(functioneditsub),functioneditsub) : "Edit"
33068         end if 
33070         if (functioneditsub:=srch(mat oas_subs,sf_cnvrtout))>0 then 
33072           print #window, fields fnfunctioneditspec$(oas_spec$(functioneditsub),functioneditsub) : "Edit"
33074         end if 
33076         if (functioneditsub:=srch(mat oas_subs,sf_picture))>0 then 
33078           print #window, fields fnfunctioneditspec$(oas_spec$(functioneditsub),functioneditsub) : "Select"
33080         end if 
33082 ! 
33084         let fncopyoasinputspec(mat oas_inputspec$,mat oas_inputdata$,mat oas_inputtooltip$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33086 ! .!
33088         do 
33090           if static_lastcontrol=control then 
33092             let curfld(static_lastcurfld)
33094           end if 
33096           print #window, fields mat oas_spec$, help mat oas_tooltip$ : mat oas_data$
33098           input #window, fields mat oas_inputspec$, help mat oas_inputtooltip$ : mat oas_inputdata$
33100           let function=fkey
33102           let fncopybackoasinputspec(mat oas_inputspec$,mat oas_inputdata$,mat oas_spec$,mat oas_data$,mat oas_subs,mat oas_buttons)
33104 ! .!
33106           let fnsetlastcurfld(control, curfld)
33108           if function=0 then 
33110             let function=1500+control
33112           end if 
33114 ! .!
33116           if function>1000 and function<=1000+udim(mat oas_subs) then 
33118             let field=function-1000
33120             if oas_subs(field) = sf_fgcolor or oas_subs(field) = sf_bgcolor then ! #Select# Oas_Subs(Field) #Case# Sf_Fgcolor # Sf_Bgcolor
33122               let fgcolorindex=srch(mat oas_subs,sf_fgcolor)
33124               let bgcolorindex=srch(mat oas_subs,sf_bgcolor)
33126 ! 
33128               if fgcolorindex>0 or bgcolorindex>0 then 
33130                 let oas_data$(field)=fncolorpicker$(oas_data$(field),0,0,"Select Color",function)
33132 ! 
33134                 if trim$(oas_data$(field))="" then ! Setting either to "" sets all to ""
33136                   if bgcolorindex>0 then let oas_data$(bgcolorindex)=""
33138                   if fgcolorindex>0 then let oas_data$(fgcolorindex)=""
33140                 else if oas_subs(field)=sf_bgcolor then 
33142                   if fgcolorindex>0 then 
33144                     if trim$(oas_data$(fgcolorindex))="" then 
33146                       let oas_data$(fgcolorindex)="W"
33148                     end if 
33150                   end if 
33152                 end if 
33154 ! 
33156                 if fgcolorindex>0 then let temp_fgcolor$=oas_data$(fgcolorindex) else let temp_fgcolor$="W"
33158                 if bgcolorindex>0 then let temp_bgcolor$=oas_data$(bgcolorindex) else let temp_bgcolor$="W"
33160 ! 
33162                 let fnadjustspeccolor(oas_spec$(field),oas_data$(field))
33164                 print #window, fields oas_spec$(field) : oas_data$(field) ! Update Button Color
33166 ! 
33168                 let fndrawcontrol(weditor,control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,temp_fgcolor$,temp_bgcolor$) ! Update Control
33170               end if 
33172 ! 
33174             else if oas_subs(field) = sf_function or oas_subs(field) = sf_cnvrtin or oas_subs(field) = sf_cnvrtout then ! #Case# Sf_Function # Sf_Cnvrtin # Sf_Cnvrtout
33176               if len(trim$(oas_data$(field))) and exists(fncustomfilenameof$(oas_data$(field))) then 
33178                 let fneditcustomfunction(oas_data$(field))
33180               else 
33182                 let oas_data$(field)=fnselectfunctiondialog$(oas_data$(field),function,2000+oas_subs(field),fnsubtype(fieldtype$(control)))
33184               end if 
33186 ! 
33188             else if oas_subs(field) = sf_picture then ! #Case# Sf_Picture
33190               if lwrc$(trim$(fieldtype$(control)))="filter" then 
33192                 let oas_data$(field)=fnselectfiltertarget$(oas_data$(field),control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
33194               else 
33196 ! .                     ! if Fnclientserver then let Csopenstring$="@:"
33198                 open #(filenumber:=fngetfilenumber): "name=open:"&csopenstring$&"*.gif;*.jpg;*.bmp;*.ico;*.png, recl=1024", external, input error ignore
33200                 if file(filenumber)=0 then 
33202                   let oas_data$(field)=file$(filenumber)
33204 ! 
33206                   let picture$(control)=oas_data$(field) ! Update Picture
33208                   let fndrawcontrol(weditor,control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
33210                 end if 
33212                 if file(filenumber)>-1 then close #filenumber: 
33214               end if 
33216 ! 
33218             else if oas_subs(field) = sf_parent then ! #Case# Sf_Parent
33220               if (lwrc$(trim$(fieldtype$(control)))="search") or (lwrc$(trim$(fieldtype$(control)))="filter") then 
33222                 let listviewindex=srch(mat fieldtype$,"LISTVIEW")
33224                 if listviewindex>0 then 
33226                   let fnsetsearchlistview(control,listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
33228                   let oas_data$(field)=parent$(control)
33230                   let specwidthindex=srch(mat oas_subs,-1*sf_specwidth)
33232                   if specwidthindex>0 then 
33234                     let oas_data$(specwidthindex)=str$(specwidth(control))
33236                   end if 
33238                   let fndrawcontrol(weditor,control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) ! Update Control
33240                 end if 
33242               end if 
33244 ! 
33246             else if oas_subs(field) = sf_fieldname then ! #Case# Sf_FieldName
33248               if lwrc$(trim$(fieldtype$(control)))="screen" then 
33250                 let screen$=fnselectscreen$
33252                 if trim$(screen$)<>"" then 
33254                   let fieldname$(control)=screen$
33256                   let oas_data$(field)=screen$
33258                   let fnreadchildscreensize(control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
33260                   let function=1500+control
33262                 end if 
33264 ! 
33266                 let fndrawcontrol(weditor,control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) ! Update Control
33268               end if 
33270 ! .!
33272             end if  ! #End Select#
33274             let fnchangeforcevisibility(0)
33276           end if 
33278 ! .!
33280         loop until function>1100 or function=99 or function=98 or function=93 or function=44 or function=19
33282         let fncopybackoasinputspec(mat oas_inputspec$,mat oas_inputdata$,mat oas_spec$,mat oas_data$,mat oas_subs,mat oas_buttons)
33284         let fnscreentoobject(control,mat oas_subs,mat oas_data$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
33286         if window and file(window)>-1 then close #window: 
33288         let fnpreformeditorinput=function
33290       else 
33292         let mode=0
33294       end if 
33296     else 
33298       let mode=0 ! Reset Mode To Default For Invalid Control Number
33300     end if 
33302   fnend 
33304 ! 
33306   dim sf_targetfield$(1)*64
33308   dim sf_filtertype$(3)
33310 ! 
33312   dim sf_spec$(3)*64, sf_data$(3)*64
33314   def fnselectfiltertarget$*255(targetstring$*255,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,count,window,savetarget$*255)
33316     open #(window:=fngetfilenumber): "srow=10,scol=30,rows=7,cols=40,border=S",display,outin 
33318     print #window, fields "1,1,CC 30;3,1,CR 15;4,1,CR 15;7,18,CC 10,/W:W,B55;7,29,CC 10,/W:W,B99" : "Select Target Field","Field: ","Filter Type: ","Ok","Cancel"
33320 ! 
33322     let targetstring$=uprc$(targetstring$)
33324     let savetarget$=targetstring$
33326 ! 
33328     if len(trim$(parent$(control))) then 
33330       let count=fncountcolumns(parent$(control),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
33332     end if 
33334     mat sf_targetfield$(count+1)
33336     let sf_targetfield$(1)="0 - All"
33338     for index=1 to count
33340       let sf_targetfield$(index+1)=str$(index)
33342     next index
33344 ! 
33346     let sf_filtertype$(1)="LEADING"
33348     let sf_filtertype$(2)="WORD"
33350     let sf_filtertype$(3)="ALL"
33352 ! 
33354     print #window, fields "3,16,15/COMBO 64,=,SELECT" : mat sf_targetfield$
33356     print #window, fields "4,16,COMBO 15,=,SELECT" : mat sf_filtertype$
33358 ! 
33360     let sf_spec$(1)="3,16,15/COMBO 64,/W:W,SELECT"
33362     let sf_spec$(2)="4,16,COMBO 15,/W:W,SELECT"
33364     let sf_spec$(3)="5,14,CHECK 15"
33366 ! 
33368     if pos(targetstring$,",") then 
33370       if lwrc$(targetstring$)(1:4)="full" then 
33372         let sf_data$(1)="0"
33374       else 
33376         let sf_data$(1)=str$(int(max(fnreturnvalue(targetstring$(1:pos(targetstring$,",")-1)),0)))
33378       end if 
33380       let targetstring$(1:pos(targetstring$,","))=""
33382       if sf_data$(1)="0" then let sf_data$(1)="0 - All"
33384     else 
33386       let sf_data$(1)=str$(int(max(fnreturnvalue(targetstring$),0)))
33388       let targetstring$=""
33390     end if 
33392 ! 
33394     if pos(targetstring$,",") then 
33396       let sf_data$(2)=targetstring$(1:pos(targetstring$,",")-1)
33398       let targetstring$(1:pos(targetstring$,","))=""
33400 ! 
33402       if sf_data$(2)<>"LEADING" and sf_data$(2)<>"WORD" and sf_data$(2)<>"ALL" then 
33404         let sf_data$(2)="ALL"
33406       end if 
33408     else 
33410       let sf_data$(2)="ALL"
33412       let targetstring$=""
33414     end if 
33416 ! 
33418     if len(targetstring$) then 
33420       let sf_data$(3)="^Case Sensitive"
33422     else 
33424       let sf_data$(3)="Case Sensitive"
33426     end if 
33428 ! 
33430     do 
33432       rinput #window, fields mat sf_spec$ : mat sf_data$
33434       if sf_data$(1)(1:1)="0" then let sf_data$(1)="0"
33436       let sf_data$(1)=str$(int(max(0,fnreturnvalue(sf_data$(1))))) ! Enforce its a positive integer
33438     loop until fkey=55 or fkey=99 or fkey=93
33440 ! 
33442     close #window: 
33444 ! 
33446     if fkey=55 then 
33448       if sf_data$(1)="0" then let savetarget$="FULLROW" else let savetarget$=sf_data$(1)
33450       let savetarget$=savetarget$&","&sf_data$(2)
33452       if sf_data$(3)(1:1)="^" then let savetarget$=savetarget$&",CASE"
33454     end if 
33456     let fnselectfiltertarget$=savetarget$
33458   fnend 
33460 ! 
33462   dim cf_target$(1)*255
33464   def fncleanfilter(&target$;___,index)
33466     let target$=uprc$(target$)
33468 ! 
33470     let str2mat(target$,mat cf_target$,",")
33472     mat cf_target$(3)
33474 ! 
33476     for index=1 to udim(mat cf_target$)
33478       let cf_target$(index)=trim$(cf_target$(index))
33480     next index
33482 ! 
33484     if lwrc$(cf_target$(1))(1:4)="full" then 
33486       let cf_target$(1)="0"
33488     else 
33490       let cf_target$(1)=str$(int(max(0,fnreturnvalue(cf_target$(1)))))
33492     end if 
33494 ! 
33496     if cf_target$(2)<>"LEADING" and cf_target$(2)<>"WORD" and cf_target$(2)<>"ALL" then 
33498       let cf_target$(2)="ALL"
33500     end if 
33502 ! 
33504     if trim$(cf_target$(3))><"" then 
33506       let cf_target$(3)="CASE"
33508     end if 
33510 ! 
33512     if val(cf_target$(1))=0 then 
33514       let cf_target$(1)="FULLROW"
33516     end if 
33518 ! 
33520     let mat2str(mat cf_target$,target$,",")
33522   fnend 
33524 ! 
33526   def fnfunctioneditspec$*80(spec$*80,index;___,row,col,length)
33528     let row=val(spec$(1:pos(spec$,",")-1))
33530     let spec$(1:pos(spec$,","))=""
33532     let col=val(spec$(1:pos(spec$,",")-1))
33534     let spec$(1:pos(spec$,","))=""
33536     if pos(spec$(1:4),"/") then 
33538       let length=val(spec$(1:pos(spec$,"/")-1)) conv ignore
33540     else 
33542       let length=val(spec$(3:pos(spec$,",")-1)) conv ignore
33544     end if 
33546 ! 
33548     let fnfunctioneditspec$=str$(row)&","&str$(col+length+1)&",4/CC 6,/W:W,B"&str$(1000+index)
33550   fnend 
33552 ! 
33554 ! 
33556 SETCURFLD: !  Set  The Curfld Of The Control
33558   def fnsetlastcurfld(control,cfld)
33560     let static_lastcontrol=control
33562     let static_lastcurfld=cfld
33564   fnend 
33566 ! 
33568 COPYOASINPUTSPEC: ! Copy The Oas Input Specs To The Input Spec Arrays
33570   def fncopyoasinputspec(mat inspec$,mat indata$, mat inhelp$, mat allspec$, mat alldata$, mat allhelp$, mat allsubs, mat buttons;___,index,count)
33572     mat inspec$(0) : mat indata$(0) : mat inhelp$(0)
33574     for index=1 to udim(mat allsubs)
33576       if ~buttons(index) then 
33578         let count=udim(mat inspec$)+1
33580         mat inspec$(count) : mat indata$(count) : mat inhelp$(count)
33582         let inspec$(count)=allspec$(index)
33584         let indata$(count)=alldata$(index)
33586         let inhelp$(count)=allhelp$(index)
33588       end if 
33590     next index
33592   fnend 
33594 ! 
33596 COPYBACKOASINPUTSPEC: ! Copy The Oas Input Specs Back To The All Spec Arrays
33598   def fncopybackoasinputspec(mat inspec$,mat indata$, mat allspec$, mat alldata$, mat allsubs, mat buttons;___,index,count)
33600     let count=0
33602     for index=1 to udim(mat allsubs)
33604       if ~buttons(index) then 
33606         let count+=1
33608         let allspec$(index)=inspec$(count)
33610         let alldata$(index)=indata$(count)
33612       end if 
33614     next index
33616   fnend 
33618 ! 
33620 ADJUSTSPECCOLOR: ! Adjust The Background Color Of The Given Spec
33622   def fnadjustspeccolor(&spec$,color$*60;___,fc$)
33624     if pos(spec$,",/") then 
33626       if fnvalidhexcolor(color$) then 
33628         if len(trim$(color$))=6 then 
33630           let spec$(pos(spec$,",/")+1:pos(spec$,",",pos(spec$,",/")+1)-1) = fnbuildcolor$(color$)
33632         else 
33634           if uprc$(trim$(color$))="T" then 
33636             let spec$(pos(spec$,",/")+1:pos(spec$,",",pos(spec$,":"))-1) = "/#FFFFFF:"&trim$(color$)
33638           else 
33640             let spec$(pos(spec$,",/")+1:pos(spec$,",",pos(spec$,":"))-1) = "/#000000:"&trim$(color$)
33642           end if 
33644         end if 
33646       else 
33648         let spec$(pos(spec$,":")+1:pos(spec$,",",pos(spec$,":"))-1) = "W"
33650       end if 
33652     end if 
33654   fnend 
33656 ! 
33658 OPENOBJECTATTRIBUTESWINDOW: !  Open The Attributes Window In A Place It Wont Be Off The Screen
33660   def fnopenobjectattributeswindow(control,rowsize,colsize,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,window,row,col,screenrowsize,screencolsize,bordersize)
33662     let row=vposition(control)
33664     let col=hposition(control)
33666     if (lwrc$(trim$(fieldtype$(control)))="frame" or lwrc$(trim$(fieldtype$(control)))="screen") and gridlines(control) then let bordersize=1
33668     let fncalculaterealposition(row, col)
33670     let fngetscreensize(screenrowsize,screencolsize)
33672     let row=row+max(height(control),1)+1+bordersize !  Try It Below First
33674     let col=col
33676     if (row)+(rowsize+1)>screenrowsize then !  If It Doesnt Fit Below
33678       let row=(row-2-bordersize)-(rowsize+2) !   Try It Above
33680     end if 
33682     if (row)+(rowsize+1)>screenrowsize then !  If It Still Doesnt Fit
33684       let row=screenrowsize-(rowsize+1) ! Move It So It Does Fit
33686     end if 
33688     if (col)+(colsize)>screencolsize then !  If It Doesnt Fit To The Right
33690       let col=col+width(control)-(colsize) !  Try It To The Left
33692     end if 
33694     if (col)+(colsize)>screencolsize then ! If It Still Doesnt Fit
33696       let col=screencolsize-(colsize+1) ! Move It So It Does Fit
33698     end if 
33700     open #(window:=fngetfilenumber) : "SROW="&str$(row)&",SCOL="&str$(col)&",ROWS="&str$(rowsize)&",COLS="&str$(colsize)&",Border=S,Caption=Control "&str$(control),display,outin 
33702     let fnopenobjectattributeswindow=window
33704   fnend 
33706 ! 
33708 CALCULATEREALPOSITION: !  Calculates The Real Position Of An Object From Window 0
33710   def fncalculaterealposition(&row,&col;___,editrow, editcol)
33712     let fngeteditorposition(editrow,editcol)
33714     let row+=editrow-1
33716     let col+=editcol-1
33718   fnend 
33720 ! 
33722 GENERATEOASPECS: ! Generate The Specs Based On Control Type)
33724   def fngenerateoaspecs(type$,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33726     let fnresetoaspecs(mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33728 ! 
33730     if trim$(lwrc$(type$)) = "c" then ! #Select# Trim$(Lwrc$(Type$))   #Case#  "c"
33732       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33734       let fnaddoaspec("Field:",sf_fieldname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33736       let fnaddoaspec("Data Width:",-1*sf_specwidth,3,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33738       let fnaddoaspec("Caption Field:",sf_description,60,mat oas_captionspec$, mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33740       let fnaddoaspec("Attributes:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33742       let fnaddoaspec("Validation:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33744       let fnaddoaspec("Foreground Color:",sf_fgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33746       let fnaddoaspec("Background Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33748       let fnaddoaspec("Justification Spec:",sf_justify,2,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33750       let fnaddoaspec("Tooltip Text:",sf_tooltip,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33752       let fnaddoaspec("Conversion:",sf_cnvrtin,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33754       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33756       let fnaddoaspec("Protected:",-1*sf_protected,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33758       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33760 ! 
33762     else if trim$(lwrc$(type$)) = "search" then ! #Case#  "search"
33764       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33766       let fnaddoaspec("Data Width:",-1*sf_specwidth,3,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33768       let fnaddoaspec("Caption Field:",sf_description,60,mat oas_captionspec$, mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33770       let fnaddoaspec("Validation:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33772       let fnaddoaspec("Foreground Color:",sf_fgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33774       let fnaddoaspec("Background Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33776       let fnaddoaspec("Attributes:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33778       let fnaddoaspec("Set Listview:",sf_parent,10,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33780       let fnaddoaspec("Tooltip Text:",sf_tooltip,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33782       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33784       let fnaddoaspec("Protected:",-1*sf_protected,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33786       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33788 ! 
33790     else if trim$(lwrc$(type$)) = "filter" then ! #Case#  "filter"
33792       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33794       let fnaddoaspec("Data Width:",-1*sf_specwidth,3,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33796       let fnaddoaspec("Caption Field:",sf_description,60,mat oas_captionspec$, mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33798       let fnaddoaspec("Validation:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33800       let fnaddoaspec("Foreground Color:",sf_fgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33802       let fnaddoaspec("Background Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33804       let fnaddoaspec("Attributes:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33806       let fnaddoaspec("Set Listview:",sf_parent,10,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33808       let fnaddoaspec("Tooltip Text:",sf_tooltip,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33810       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33812       let fnaddoaspec("Protected:",-1*sf_protected,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33814       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33816       let fnaddoaspec("Target Details:",sf_picture,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Click this button to set the selection type for BR to use for the listview. You can also type the selection string directly in here.")
33818 ! 
33820     else if trim$(lwrc$(type$)) = "caption" then ! #Case#  "caption"
33822       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33824       let fnaddoaspec("Caption:",sf_description,60,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"The text to display.")
33826       let fnaddoaspec("Field:",sf_fieldname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Use this to tie the caption to a field on disk. Leave this blank if you hardcode the caption.")
33828       let fnaddoaspec("Function:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Click Event Function. The function specified here will be run whenever the user clicks on the control.")
33830       let fnaddoaspec("Foreground Color:",sf_fgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33832       let fnaddoaspec("Background Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33834       let fnaddoaspec("Justification Spec:",sf_justify,2,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33836       let fnaddoaspec("Attributes:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33838       let fnaddoaspec("Tooltip Text:",sf_tooltip,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33840       let fnaddoaspec("Conversion:",sf_cnvrtin,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33842       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33844       let fnaddoaspec("Protected:",-1*sf_protected,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33846       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33848 ! .       !
33850     else if trim$(lwrc$(type$)) = "p" then ! #Case# "p"
33852       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33854       let fnaddoaspec("Picture File:",sf_picture,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Enter or select the image file to display.")
33856       let fnaddoaspec("Caption:",sf_description,60,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"The caption to display with the picture.")
33858       let fnaddoaspec("Field:",sf_fieldname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Use this to tie the picture to a field on disk. Leave this blank if you hardcode the picture.")
33860       let fnaddoaspec("Attributes:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33862       let fnaddoaspec("Function:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Click Event Function. The function specified here will be run whenever the user clicks on the control.")
33864       let fnaddoaspec("Background Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33866       let fnaddoaspec("Tooltip Text:",sf_tooltip,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33868       let fnaddoaspec("Conversion:",sf_cnvrtin,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33870       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33872       let fnaddoaspec("Protected:",-1*sf_protected,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33874       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33876 ! 
33878     else if trim$(lwrc$(type$)) = "calendar" then ! #Case# "calendar"
33880       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33882       let fnaddoaspec("Field:",sf_fieldname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33884       let fnaddoaspec("Target Control:",sf_picture,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33886       let fnaddoaspec("Caption:",sf_description,60,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33888       let fnaddoaspec("Attributes:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33890       let fnaddoaspec("Function:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Click Event Function. The function specified here will be run whenever the user clicks on the control.")
33892       let fnaddoaspec("Background Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33894       let fnaddoaspec("Tooltip Text:",sf_tooltip,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33896       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33898       let fnaddoaspec("Protected:",-1*sf_protected,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33900       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33902 ! .      ! Needs a button to select the Target Control
33904 ! .      ! or perhaps just specify the function or just have it generate a picture with a function.
33906 ! .      !  it would be just as simple to say in the "Generate", "please select a target control." and then it just generates a P control
33908 ! .      !  with the calendar icon for the image and the function that calls mikhails function.
33910 ! 
33912 ! 
33914     else if trim$(lwrc$(type$)) = "check" then ! #Case# "check"
33916       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33918       let fnaddoaspec("Field:",sf_fieldname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33920       let fnaddoaspec("Caption:",sf_description,60,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"The text to display with the checkbox.")
33922       let fnaddoaspec("Truth Value:",sf_truevalue,60,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33924       let fnaddoaspec("Falsehood Value:",sf_falsevalue,60,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33926       let fnaddoaspec("Validation:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33928       let fnaddoaspec("Attributes:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33930       let fnaddoaspec("Foreground Color:",sf_fgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33932       let fnaddoaspec("Background Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33934       let fnaddoaspec("Tooltip Text:",sf_tooltip,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33936       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33938       let fnaddoaspec("Protected:",-1*sf_protected,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33940       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33942 ! 
33944     else if trim$(lwrc$(type$)) = "button" then ! #Case#  "button"
33946       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33948       let fnaddoaspec("Caption:",sf_description,60,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"The text to display on the button.")
33950       let fnaddoaspec("Field:",sf_fieldname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Use this to tie the buttons Caption to a field on disk. Leave this blank if you hardcode the caption.")
33952       let fnaddoaspec("Function:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Click Event Function. The function specified here will be run whenever the user clicks on the button.")
33954       let fnaddoaspec("Attributes:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33956       let fnaddoaspec("Foreground Color:",sf_fgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33958       let fnaddoaspec("Background Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33960       let fnaddoaspec("Tooltip Text:",sf_tooltip,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33962       let fnaddoaspec("Conversion:",sf_cnvrtin,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33964       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33966       let fnaddoaspec("Protected:",-1*sf_protected,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33968       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33970 ! 
33972     else if trim$(lwrc$(type$)) = "listview" then ! #Case# "listview"
33974       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33976       let fnaddoaspec("Caption Field:",sf_description,60,mat oas_captionspec$, mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33978       let fnaddoaspec("Sort Column:",-1*sf_specwidth,2,mat oas_captionspec$, mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"The sort column for the listview. Recommended if there is a search box, as BR uses this for the field to search.")
33980       let fnaddoaspec("Filter:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Filter Function for the listview. This function will be called once for each record in the data file. Return true to include it and false to exclude it from the listview. See the documentation for more details.")
33982       let fnaddoaspec("Header FG Color:",sf_fgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Click to select the Foreground Color for the listview headers.")
33984       let fnaddoaspec("Header BG Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Click to select the Background Color for the listview headers.")
33986       let fnaddoaspec("Attributes:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Business Rules Control Attributes for the listview headers. This can be any valid BR Attribute specification for Listviews.")
33988       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33990       let fnaddoaspec("Protected:",-1*sf_protected,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33992       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33994       let fnaddoaspec("MultiSelect:",-1*sf_multiselect,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
33996       if fn42 then 
33998         let fnaddoaspec("Gridlines:",-1*sf_gridlines,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34000       end if 
34002 ! 
34004     else if trim$(lwrc$(type$)) = "frame" then ! #Case#  "frame"
34006       let fnaddoaspec("Border:",-1*sf_gridlines,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Put a border on the frame.")
34008       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34010       let fnaddoaspec("Caption:",sf_description,60,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Caption for the frame's Border.")
34012       let fnaddoaspec("Picture File:",sf_picture,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Picture to use for background of frame. Use this feature to place controls on top of pictures.")
34014       let fnaddoaspec("Foreground Color:",sf_fgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34016       let fnaddoaspec("Background Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34018       let fnaddoaspec("Color Attr:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Color Attributes for the frame.")
34020       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34022       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34024 ! .       !
34026     else if trim$(lwrc$(type$)) = "screen" then ! #Case# "screen"
34028       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34030       let fnaddoaspec("Screen:",sf_fieldname,18,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,1,"Select the target screen here.")
34032       let fnaddoaspec("Function:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Use this field to specify any values that need to be passed into the child screen. Example: ""ParentKey$=CurrentKey$"".")
34034       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34036       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34038 ! 
34040     else if trim$(lwrc$(type$)) = "combo" then ! #Case#  "combo"
34042       let fnaddoaspec("Name:",sf_controlname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34044       let fnaddoaspec("Field:",sf_fieldname,50,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34046       let fnaddoaspec("Data Width:",-1*sf_specwidth,3,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34048       let fnaddoaspec("Caption Field:",sf_description,60,mat oas_captionspec$, mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34050       let fnaddoaspec("Attributes:",sf_attr,128,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34052       let fnaddoaspec("Populate:",sf_function,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Populate and Validate event function. This function will be run when ScreenIO tries to populate the combo box, and any data placed in an array called mat ReturnData$ will be put in the combo box.")
34054       let fnaddoaspec("Foreground Color:",sf_fgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34056       let fnaddoaspec("Background Color:",sf_bgcolor,6,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34058       let fnaddoaspec("Tooltip Text:",sf_tooltip,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34060       let fnaddoaspec("Conversion:",sf_cnvrtin,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34062       let fnaddoaspec("User Data:",sf_userdata,255,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34064       let fnaddoaspec("Protected:",-1*sf_protected,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34066       let fnaddoaspec("Invisible:",-1*sf_invisible,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons)
34068       if fn42 then 
34070         let fnaddoaspec("Select Only:",-1*sf_gridlines,1,mat oas_captionspec$,mat oas_captiondata$,mat oas_spec$,mat oas_data$,mat oas_tooltip$,mat oas_subs,mat oas_buttons,0,"Select this to make a Selection Box instead of a Combo Box.")
34072       end if 
34074     end if  ! #End Select#
34076   fnend 
34078 ! 
34080 RESETOASPECS: ! Reset Oa Specs
34082   def fnresetoaspecs(mat cspec$,mat cdata$,mat spec$,mat dat$,mat ttip$,mat subs,mat buttons)
34084 ! 
34086     mat cspec$(0)
34088     mat cdata$(0)
34090     mat spec$(0)
34092     mat dat$(0)
34094     mat ttip$(0)
34096     mat subs(o)
34098     mat buttons(0)
34100 ! 
34102   fnend 
34104 ! 
34106   def fndefaulttooltip$*255(sub)
34108     if sub = sf_controlname then ! #Select# Sub #Case# Sf_ControlName
34110       let fndefaulttooltip$="Control Name. Use to reference control in code, example: let Invisible(ctl_ControlName)=0"
34112     else if sub = sf_fieldname then ! #Case# Sf_FieldName
34114       let fndefaulttooltip$="Field Name of the field in the data file for when control is tied directly to data file."
34116     else if sub = -1*sf_specwidth then ! #Case# -1*Sf_SpecWidth
34118       let fndefaulttooltip$="Maximum allowed width for the data. The user will be stopped from typing more characters then this."
34120     else if sub = sf_description then ! #Case# Sf_Description
34122       let fndefaulttooltip$="Control Name of the control that is the caption for this control. The specified control's movement will be tied to this control, to make screen design more convenient."
34124     else if sub = sf_attr then ! #Case# Sf_Attr
34126       let fndefaulttooltip$="Business Rules Control Attributes for the control. This can be references to attributes defined in your brconfig.sys file, or hardcoded control attributes."
34128     else if sub = sf_function then ! #Case# Sf_Function
34130       let fndefaulttooltip$="Validation function for the control. The selected function will be run any time the data in this control changes. This can be used to update related fields on the screen."
34132     else if sub = sf_fgcolor then ! #Case# sf_fgcolor
34134       let fndefaulttooltip$="Click to select the Foreground Color for the control."
34136     else if sub = sf_bgcolor then ! #Case# sf_bgcolor
34138       let fndefaulttooltip$="Click to select the Background Color for the control."
34140     else if sub = sf_justify then ! #Case# Sf_Justify
34142       let fndefaulttooltip$="Business Rules Justification Spec: C=Center, R=Right, L=Lowercase and U=Uppercase."
34144     else if sub = sf_tooltip then ! #Case# Sf_Tooltip
34146       let fndefaulttooltip$="Help text to be shown when the user hovers the mouse over this control."
34148     else if sub = sf_cnvrtin then ! #Case# Sf_CnvrtIn
34150       let fndefaulttooltip$="ScreenIO Conversion Spec, choose from any valid FMT(, PIC( or DATE( spec."
34152     else if sub = sf_userdata then ! #Case# Sf_UserData
34154       let fndefaulttooltip$="mat UserData$( value for the control. You can use this to communicate information about the control to your custom event functions. This value is not used by ScreenIO."
34156     else if sub = -1*sf_protected then ! #Case# -1*Sf_Protected
34158       let fndefaulttooltip$="If this is checked, the control will be kept out of the input fields statement and will be read only to the user."
34160     else if sub = -1*sf_invisible then ! #Case# -1*Sf_Invisible
34162       let fndefaulttooltip$="If this is checked, the control will be kept off the screen, making it invisible to the user."
34164     else if sub = sf_parent then ! #Case# Sf_Parent
34166       let fndefaulttooltip$="Click this button to tie the filter/search box to a listview. Should be done automatically if you added the listview first."
34168     else if sub = sf_truevalue then ! #Case# Sf_TrueValue
34170       let fndefaulttooltip$="The value to save in the data if the box is checked. (Used when reading too.)"
34172     else if sub = sf_falsevalue then ! #Case# Sf_FalseValue
34174       let fndefaulttooltip$="The value to save in the data if the box is unchecked. (Used when reading too.)"
34176     else if sub = -1*sf_gridlines then ! #Case# -1*Sf_GridLines
34178       let fndefaulttooltip$="Draw Gridlines to make a listview look like a grid."
34180     else if sub = -1*sf_multiselect then ! #Case# -1*Sf_MultiSelect
34182       let fndefaulttooltip$="Check this checkbox to make a multiselect listview. Use mat SelectedKeys$ in your functions to see which records are selected."
34184     end if  ! #End Select#
34186   fnend 
34188 ! 
34190 ! 
34192 ADDOASPEC: ! Add A Spec
34194   def fnaddoaspec(caption$*30,sub,width,mat cspec$,mat cdata$,mat spec$,mat dat$, mat ttip$,mat subs, mat buttons;forcebutton,tooltip$*255,___,index)
34196 ! 
34198     let index=udim(mat cspec$)+1
34200     mat cspec$(index)
34202     mat cdata$(index)
34204     mat spec$(index)
34206     mat dat$(index)
34208     mat ttip$(index)
34210     mat subs(index)
34212     mat buttons(index)
34214 ! 
34216     if tooltip$="" then 
34218       let tooltip$=fndefaulttooltip$(sub)
34220     end if 
34222 ! 
34224     let fnfixtooltips(tooltip$)
34226 ! 
34228     let cspec$(index)=str$(index)&",1,"&str$(int(len(caption$)*4/5)+2)&"/CL "&str$(len(caption$))
34230     let cdata$(index)=caption$
34232     let ttip$(index)=tooltip$
34234     let subs(index)=sub
34236     let buttons(index)=0
34238 ! 
34240     if sub = sf_fgcolor or sub = sf_bgcolor or sub = sf_parent then ! #Select# Sub #Case# Sf_Fgcolor # Sf_Bgcolor # Sf_Parent
34242       let spec$(index)=str$(index)&","&str$(int(len(caption$)*4/5)+4)&","&str$(min(32-(int(len(caption$)*4/5)+2),width+3))&"/CC "&str$(width)&",/W:W,B"&str$(1000+index)
34244       let buttons(index)=1
34246 ! 
34248     else if sub = sf_justify then ! #Case# Sf_Justify
34250       let spec$(index)=str$(index)&","&str$(int(len(caption$)*4/5)+3)&","&str$(min(32-(int(len(caption$)*4/5)+2),width+3))&"/COMBO "&str$(width)&",/W:#FFFF77"
34252 ! 
34254     else if sub = sf_function or sub = sf_cnvrtin or sub = sf_cnvrtout or sub = sf_picture then ! #Case# Sf_Function # Sf_Cnvrtin # Sf_Cnvrtout # Sf_Picture
34256       let spec$(index)=str$(index)&","&str$(int(len(caption$)*4/5)+3)&","&str$(32-(int(len(caption$)*4/5)+8))&"/V "&str$(width)&",S/W:#FFFF77"
34258 ! 
34260     else if sub = -1*sf_protected or sub = -1*sf_invisible or sub = -1*sf_multiselect or sub = -1*sf_gridlines then ! #Case# -1*Sf_Protected # -1*Sf_Invisible # -1*Sf_Multiselect # -1*Sf_GridLines
34262       let spec$(index)=str$(index)&","&str$(int(len(caption$)*4/5)+3)&","&str$(32-(int(len(caption$)*4/5)+2))&"/CHECK "&str$(width)
34264 ! 
34266     else ! #Case Else#
34268       let spec$(index)=str$(index)&","&str$(int(len(caption$)*4/5)+3)&","&str$(32-(int(len(caption$)*4/5)+2))&"/V "&str$(width)&",S/W:#FFFF77"
34270 ! 
34272       if forcebutton then 
34274         let spec$(index)=str$(index)&","&str$(int(len(caption$)*4/5)+4)&","&str$(30-(int(len(caption$)*4/5)+2))&"/CC "&str$(width)&",/W:W,B"&str$(1000+index)
34276         let buttons(index)=1
34278       end if 
34280 ! 
34282     end if  ! #End Select#
34284 ! 
34286   fnend 
34288 ! 
34290 OBJECTTOSCREEN: ! Put Object Data Into Input Data Based On Type
34292   def fnobjecttoscreen(control,mat oas_subs,mat oas_data$,mat oas_spec$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
34294     for index=1 to udim(mat oas_subs)
34296       if oas_subs(index) = sf_controlname then ! #Select# Oas_Subs(Index)   #Case# Sf_Controlname
34298         let oas_data$(index)=controlname$(control)
34300 ! 
34302       else if oas_subs(index) = sf_fieldname then ! #Case# Sf_Fieldname
34304         let oas_data$(index)=fieldname$(control)
34306 ! 
34308       else if oas_subs(index) = sf_description then ! #Case# Sf_Description
34310         let oas_data$(index)=description$(control)
34312 ! 
34314       else if oas_subs(index) = -1*sf_specwidth then ! #Case# -1*Sf_Specwidth
34316         let oas_data$(index)=str$(specwidth(control))
34318 ! 
34320       else if oas_subs(index) = sf_truevalue then ! #Case# Sf_Truevalue
34322         let oas_data$(index)=truevalue$(control)
34324 ! 
34326       else if oas_subs(index) = sf_falsevalue then ! #Case# Sf_Falsevalue
34328         let oas_data$(index)=falsevalue$(control)
34330 ! 
34332       else if oas_subs(index) = sf_function then ! #Case# Sf_Function
34334         let oas_data$(index)=function$(control)
34336 ! 
34338       else if oas_subs(index) = sf_picture then ! #Case# Sf_Picture
34340         let oas_data$(index)=picture$(control)
34342 ! 
34344       else if oas_subs(index) = sf_fgcolor then ! #Case# Sf_Fgcolor
34346         let oas_data$(index)=fgcolor$(control)
34348         let fnadjustspeccolor(oas_spec$(index),oas_data$(index))
34350 ! 
34352       else if oas_subs(index) = sf_bgcolor then ! #Case# Sf_Bgcolor
34354         let oas_data$(index)=bgcolor$(control)
34356         let fnadjustspeccolor(oas_spec$(index),oas_data$(index))
34358 ! 
34360       else if oas_subs(index) = sf_justify then ! #Case# Sf_Justify
34362         let oas_data$(index)="C"&justify$(control)
34364 ! 
34366       else if oas_subs(index) = sf_parent then ! #Case# Sf_Parent
34368         let oas_data$(index)=parent$(control)
34370 ! 
34372       else if oas_subs(index) = sf_attr then ! #Case# Sf_Attr
34374         let oas_data$(index)=attr$(control)
34376 ! 
34378       else if oas_subs(index) = -1*sf_protected then ! #Case# -1*Sf_Protected
34380         if protected(control) then 
34382           let oas_data$(index)="^"
34384         else 
34386           let oas_data$(index)=""
34388         end if 
34390 ! 
34392       else if oas_subs(index) = -1*sf_invisible then ! #Case# -1*Sf_Invisible
34394         if invisible(control) then 
34396           let oas_data$(index)="^"
34398         else 
34400           let oas_data$(index)=""
34402         end if 
34404 ! 
34406       else if oas_subs(index) = sf_tooltip then ! #Case# Sf_Tooltip
34408         let oas_data$(index)=tooltip$(control)
34410 ! 
34412       else if oas_subs(index) = sf_cnvrtin then ! #Case# Sf_Cnvrtin
34414         let oas_data$(index)=cnvrtin$(control)
34416 ! 
34418       else if oas_subs(index) = sf_cnvrtout then ! #Case# Sf_Cnvrtout
34420         let oas_data$(index)=cnvrtout$(control)
34422 ! 
34424       else if oas_subs(index) = -1*sf_multiselect then ! #Case# -1*Sf_Multiselect
34426         if multiselect(control) then 
34428           let oas_data$(index)="^"
34430         else 
34432           let oas_data$(index)=""
34434         end if 
34436 ! 
34438       else if oas_subs(index) = -1*sf_gridlines then ! #Case# -1*Sf_GridLines
34440         if gridlines(control) then 
34442           let oas_data$(index)="^"
34444         else 
34446           let oas_data$(index)=""
34448         end if 
34450 ! 
34452       else if oas_subs(index) = sf_userdata then ! #Case# Sf_Userdata
34454         let oas_data$(index)=userdata$(control)
34456 ! 
34458       end if  ! #End Select#
34460     next index
34462   fnend 
34464 ! 
34466 SCREENTOOBJECT: !  Put Input Data Back In  The Objects
34468   def fnscreentoobject(control,mat oas_subs,mat oas_data$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,tempwidth,choice,askedalready,temp)
34470 ! 
34472     for index=1 to udim(mat oas_subs)
34474       if oas_subs(index) = sf_controlname then ! #Select# Oas_Subs(Index)   #Case#    Sf_Controlname
34476         let fnsavetoarray$(control,mat controlname$,srep$(trim$(oas_data$(index))," ","_"))
34478 ! 
34480       else if oas_subs(index) = sf_fieldname then ! #Case# Sf_Fieldname
34482         let fnsavetoarray$(control,mat fieldname$,oas_data$(index))
34484 ! 
34486       else if oas_subs(index) = -1*sf_specwidth then ! #Case# -1*Sf_Specwidth
34488         let tempwidth=-999
34490         let tempwidth = val(oas_data$(index)) conv ignore
34492         if tempwidth>-999 then 
34494           if lwrc$(trim$(fieldtype$(control)))="listview" then 
34496             let temp=tempwidth
34498           else 
34500             let temp=max(tempwidth,1)
34502           end if 
34504           let fnsavetoarray(control,mat specwidth,temp)
34506         end if 
34508 ! 
34510       else if oas_subs(index) = sf_description then ! #Case# Sf_Description
34512         let fnsavetoarray$(control,mat description$,oas_data$(index))
34514 ! 
34516       else if oas_subs(index) = sf_truevalue then ! #Case# Sf_Truevalue
34518         let fnsavetoarray$(control,mat truevalue$,oas_data$(index))
34520 ! 
34522       else if oas_subs(index) = sf_falsevalue then ! #Case# Sf_Falsevalue
34524         let fnsavetoarray$(control,mat falsevalue$,oas_data$(index))
34526 ! 
34528       else if oas_subs(index) = sf_function then ! #Case# Sf_Function
34530         let fnsavetoarray$(control,mat function$,oas_data$(index))
34532 ! 
34534       else if oas_subs(index) = sf_picture then ! #Case# Sf_Picture
34536         if lwrc$(trim$(fieldtype$(control)))="filter" then 
34538           let fncleanfilter(oas_data$(index))
34540         end if 
34542         let fnsavetoarray$(control,mat picture$,oas_data$(index))
34544 ! 
34546       else if oas_subs(index) = sf_fgcolor then ! #Case# Sf_Fgcolor
34548         if trim$(oas_data$(index))="" or fnvalidhexcolor(oas_data$(index)) then 
34550           let fnsavetoarray$(control,mat fgcolor$,oas_data$(index))
34552         end if 
34554 ! 
34556       else if oas_subs(index) = sf_bgcolor then ! #Case# Sf_Bgcolor
34558         if trim$(oas_data$(index))="" or fnvalidhexcolor(oas_data$(index)) then 
34560           let fnsavetoarray$(control,mat bgcolor$,oas_data$(index))
34562         end if 
34564 ! 
34566       else if oas_subs(index) = sf_justify then ! #Case# Sf_Justify
34568         if fnvalidjustify(oas_data$(index)) then 
34570           let fnsavetoarray$(control,mat justify$,trim$(oas_data$(index)(2:2)))
34572         end if 
34574 ! 
34576       else if oas_subs(index) = sf_parent then ! #Case# Sf_Parent
34578         if fnfindtarget(oas_data$(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
34580           let fnsavetoarray$(control,mat parent$,oas_data$(index))
34582         end if 
34584 ! 
34586       else if oas_subs(index) = sf_attr then ! #Case# Sf_Attr
34588         let fnsavetoarray$(control,mat attr$,oas_data$(index))
34590 ! 
34592       else if oas_subs(index) = -1*sf_protected then ! #Case# -1*Sf_Protected
34594         if oas_data$(index)(1:1)="^" then 
34596           let temp=1
34598         else 
34600           let temp=0
34602         end if 
34604         let fnsavetoarray(control,mat protected,temp)
34606 ! 
34608       else if oas_subs(index) = -1*sf_invisible then ! #Case# -1*Sf_Invisible
34610         if oas_data$(index)(1:1)="^" then 
34612           let temp=1
34614         else 
34616           let temp=0
34618         end if 
34620         let fnsavetoarray(control,mat invisible,temp)
34622 ! 
34624       else if oas_subs(index) = sf_tooltip then ! #Case# Sf_Tooltip
34626         let fnsavetoarray$(control,mat tooltip$,oas_data$(index))
34628 ! 
34630       else if oas_subs(index) = sf_cnvrtin then ! #Case# Sf_Cnvrtin
34632         let fnsavetoarray$(control,mat cnvrtin$,oas_data$(index))
34634 ! 
34636       else if oas_subs(index) = sf_cnvrtout then ! #Case# Sf_Cnvrtout
34638         let fnsavetoarray$(control,mat cnvrtout$,oas_data$(index))
34640 ! 
34642       else if oas_subs(index) = -1*sf_multiselect then ! #Case# -1*Sf_Multiselect
34644         if oas_data$(index)(1:1)="^" then let temp=1 else let temp=0
34646         let fnsavetoarray(control,mat multiselect,temp)
34648 ! 
34650       else if oas_subs(index) = -1*sf_gridlines then ! #Case# -1*Sf_GridLines
34652         if oas_data$(index)(1:1)="^" then let temp=1 else let temp=0
34654         let fnsavetoarray(control,mat gridlines,temp)
34656 ! 
34658       else if oas_subs(index) = sf_userdata then ! #Case# Sf_Userdata
34660         let fnsavetoarray$(control,mat userdata$,oas_data$(index))
34662 ! 
34664       end if  ! #End Select#
34666     next index
34668   fnend 
34670 ! 
34672   def fnsavetoarray$(control,mat array$,value$*255;___,index)
34674     if trim$(value$)<>trim$(array$(control)) then 
34676       let array$(control)=value$
34678       if udim(mat selectedcontrols)>=control and selectedcontrols(control) then 
34680         if ~askedalready then 
34682           let choice=msgbox("Would you like to copy the changes to all selected controls?","Change Many?","yN","QST")
34684           let askedalready=1
34686         end if 
34688         if choice=2 then 
34690           for index=1 to udim(mat selectedcontrols)
34692             if selectedcontrols(index) then 
34694               let array$(index)=value$
34696             end if 
34698           next index
34700         end if 
34702       end if 
34704     end if 
34706   fnend 
34708   def fnsavetoarray(control,mat array,value;___,index)
34710     if value<>array(control) then 
34712       let array(control)=value
34714       if udim(mat selectedcontrols)>=control and selectedcontrols(control) then 
34716         if ~askedalready then 
34718           let choice=msgbox("Would you like to copy the changes to all selected controls?","Change Many?","yN","QST")
34720           let askedalready=1
34722         end if 
34724         if choice=2 then 
34726           for index=1 to udim(mat selectedcontrols)
34728             if selectedcontrols(index) then 
34730               let array(index)=value
34732             end if 
34734           next index
34736         end if 
34738       end if 
34740     end if 
34742   fnend 
34744 ! 
34746   dim justifyoptions$(1)
34748 VALIDJUSTIFY: ! Validates The Justification Selection
34750   def fnvalidjustify(justify$)
34752     if srch(mat justifyoptions$,trim$(justify$))>0 then let fnvalidjustify=1
34754   fnend 
34756 ! 
34758 FINDTARGET: ! 
34760   def fnfindtarget(parent$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
34762     for index=1 to udim(mat fieldtype$)
34764       if fieldtype$(index)="LISTVIEW" and parent$(index)=parent$ then 
34766         let fnfindtarget=index
34768       end if 
34770     next index
34772   fnend 
34774 ! 
34776 POPULATEJUSTIFY: ! Populate The Justification Textbox
34778   def fnpopulatejustify(spec$*40,window)
34780     mat justifyoptions$(5)
34782     let justifyoptions$(1)="C"
34784     let justifyoptions$(2)="CC"
34786     let justifyoptions$(3)="CR"
34788     let justifyoptions$(4)="CU"
34790     let justifyoptions$(5)="CL"
34792 ! 
34794     print #window, fields spec$(1:pos(spec$,",",-1))&"=" : mat justifyoptions$
34796   fnend 
34798 ! 
34800   def fnfixallchildscreensize(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
34802     for index=1 to udim(mat fieldtype$)
34804       if lwrc$(trim$(fieldtype$(index))) = "screen" then ! #Select# lwrc$(trim$(FieldType$(Index))) #Case# "screen"
34806         let fnreadchildscreensize(index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
34808       end if  ! #End Select#
34810     next index
34812   fnend 
34814 ! 
34816   def fnreadchildscreensize(index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,bordersize,screenattr$*255)
34818     let width(index)=fnreadunopenednumber("screenio",trim$(fieldname$(index)),si_hsize)
34820     let height(index)=fnreadunopenednumber("screenio",trim$(fieldname$(index)),si_vsize)
34822     let gridlines(index)=fnreadunopenednumber("screenio",trim$(fieldname$(index)),si_border)
34824     let screenattr$=fnreadunopeneddescription$("screenio",trim$(fieldname$(index)),si_attributes)
34826 ! 
34828     if pos(lwrc$(trim$(screenattr$)),"border") then let gridlines(index)=1
34830 ! 
34832     if gridlines(index) then let bordersize=2
34834 ! 
34836     if width(index)+bordersize>screenio(si_hsize) then let screenio(si_hsize)=width(index)+bordersize
34838     if height(index)+bordersize>screenio(si_vsize) then let screenio(si_vsize)=height(index)+bordersize
34840     let vposition(index)=min(vposition(index),screenio(si_vsize)-height(index)+1-(bordersize/2))
34842     let hposition(index)=min(hposition(index),screenio(si_hsize)-width(index)+1-(bordersize/2))
34844   fnend 
34846 ! 
35000 ! #Autonumber# 35000,1
35001 ! =============================== File Read/Write ===========================
35002 ! = This Next Section And These Variables Are All For File Reads And Writes =
35003 ! ===========================================================================
35004   dim screenfld$(1)*255, screenfld(1)
35005   dim tempscreenfld$(1)*255, tempscreenfld(1)
35006   dim tempscreenio$(1)*255, tempscreenio(1)
35007   dim fscreenio, fscreenfld
35008   dim static_screenname$
35009 READSCREEN: ! Read The Screen And Populate The Given Arrays
35010   def fnreadscreen(screenname$,mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;skippreserve,___, index)
35011     let screenname$=uprc$(trim$(screenname$))
35012     if trim$(screenname$)="" then 
35013 ! 
35014       mat screenio$=("") : mat screenio=(0)
35015       let fnresizecontrolarrays(0, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35016 ! 
35017       let screenio(si_vsize)=24
35018       let screenio(si_hsize)=80
35019 ! 
35020       let control=0
35021 ! 
35022       let fnreadscreen=1 ! New Screen Initialized
35023       let static_screenname$=screenname$
35024       let fnpreservescreenarrays(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35025     else 
35026 ! .   ! Load Screen
35027       read #fscreenio, using form$(fscreenio), key=fnkey$(fscreenio,screenname$), release: mat screenio$, mat screenio nokey ignore
35028       if file(fscreenio)=0 then 
35029 ! 
35030         restore #fscreenfld, key=fnkey$(fscreenfld,screenname$): nokey ignore
35031 ! 
35032         let fnresizecontrolarrays(0, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35033 ! 
35034         do while file(fscreenfld)=0
35035           read #fscreenfld, using form$(fscreenfld), release: mat screenfld$, mat screenfld eof ignore
35036 ! 
35037           if file(fscreenfld)=0 and trim$(screenfld$(sf_screencode))=trim$(screenname$) then 
35038             let index=udim(mat controlname$)+1
35039             let fnresizecontrolarrays(index, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35040 ! 
35041             let controlname$(index)=screenfld$(sf_controlname)
35042             let fieldname$(index)=screenfld$(sf_fieldname)
35043             let description$(index)=screenfld$(sf_description)
35044             let vposition(index)=screenfld(sf_vposition)
35045             let hposition(index)=screenfld(sf_hposition)
35046             let fieldtype$(index)=screenfld$(sf_fieldtype)
35047             let specwidth(index)=screenfld(sf_specwidth)
35048             let width(index)=screenfld(sf_width)
35049             let height(index)=screenfld(sf_height)
35050             let truevalue$(index)=screenfld$(sf_truevalue)
35051             let falsevalue$(index)=screenfld$(sf_falsevalue)
35052             let function$(index)=screenfld$(sf_function)
35053             let picture$(index)=screenfld$(sf_picture)
35054             let parent$(index)=screenfld$(sf_parent)
35055             let fgcolor$(index)=screenfld$(sf_fgcolor)
35056             let bgcolor$(index)=screenfld$(sf_bgcolor)
35057             let justify$(index)=screenfld$(sf_justify)
35058             let attr$(index)=screenfld$(sf_attr)
35059             let protected(index)=screenfld(sf_protected)
35060             let invisible(index)=screenfld(sf_invisible)
35061             let tooltip$(index)=screenfld$(sf_tooltip)
35062             let cnvrtin$(index)=screenfld$(sf_cnvrtin)
35063             let cnvrtout$(index)=screenfld$(sf_cnvrtout)
35064             let multiselect(index)=screenfld(sf_multiselect)
35065             let gridlines(index)=screenfld(sf_gridlines)
35066             let userdata$(index)=screenfld$(sf_userdata)
35067           end if 
35068         loop while trim$(screenfld$(sf_screencode))=trim$(screenname$)
35069         let fnreadscreen=1 !  Screen Read Successful
35070         if ~skippreserve then 
35071           let static_screenname$=screenname$
35072           let fnpreservescreenarrays(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35073         end if 
35074         let fnfixallchildscreensize(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35075       else 
35076         let fnreadscreen=0 !  Screen Not Found
35077       end if 
35078     end if 
35079   fnend 
35080 ! 
35081   def fnlaunchscreen(screencode$;___,filenumber,designerpath$)
35082 ! .   ! Build a proc to run the screen
35083     open #(filenumber:=fngetfilenumber): "name=launch"&session$&".$$$, replace", display, output 
35084     print #filenumber: "proc noecho"
35085     if exists(srep$(setting_screeniopath$&".br","screenio.br","design.br")) then 
35086       print #filenumber: "load "&srep$(setting_screeniopath$&".br","screenio.br","design")
35087     else 
35088       print #filenumber: "load "&setting_screeniopath$
35089     end if 
35090     print #filenumber: "5 let fnDesignScreen("""&screencode$&""")"
35091     print #filenumber: "6 execute ""system"""
35092     print #filenumber: "run"
35093     close #filenumber: 
35094 ! 
35095     execute "system -C -M "&fngetbrexe$&" -"&fngetwbcfg$&" proc launch"&session$&".$$$"
35096   fnend 
35097 ! 
35098 WRITESCREEN: !  Write The Results To Disk
35099   def fnwritescreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;waitforcomplete,___,index,writeerror,backupfile)
35100 ! 
35101     if len(trim$(screenio$(si_screencode)))>0 then 
35102 ! 
35103       let fnwritebackup(mat screenio$,mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35104 ! 
35105       let screenio(si_modifydate)=days(date$("mm/dd/ccyy"),"mm/dd/ccyy")
35106       if (trim$(static_screenname$)<>trim$(screenio$(si_screencode))) then 
35107         restore #fscreenio, key=fnkey$(fscreenio,screenio$(si_screencode)): nokey ignore
35108         if file(fscreenio)<>0 then 
35109           let screenio(si_createdate)=days(date$("mm/dd/ccyy"),"mm/dd/ccyy")
35110           write #fscreenio, using form$(fscreenio) : mat screenio$, mat screenio
35111           let writeerror=0
35112         else 
35113           let choice=msgbox(trim$(screenio$(si_screencode))&" already exists. Do you wish to overwrite it?","Confirm overwrite","yN","QST")
35114           if choice=2 then ! 2 Is Yes
35115             rewrite #fscreenio, using form$(fscreenio),key=fnkey$(fscreenio,screenio$(si_screencode)) : mat screenio$, mat screenio
35116             let fndeleteallfields(screenio$(si_screencode))
35117             let writeerror=0
35118           else 
35119             let writeerror=1
35120           end if 
35121         end if 
35122       else 
35123         restore #fscreenio, key=fnkey$(fscreenio,screenio$(si_screencode)): nokey ignore
35124         if file(fscreenio)=0 then 
35125           rewrite #fscreenio, using form$(fscreenio), key=fnkey$(fscreenio,screenio$(si_screencode)) : mat screenio$, mat screenio
35126           let fndeleteallfields(screenio$(si_screencode))
35127           let writeerror=0
35128         else ! Record Must Have Been Deleted By An Outside Process
35129           write #fscreenio, using form$(fscreenio) : mat screenio$, mat screenio
35130           let fndeleteallfields(screenio$(si_screencode))
35131           let writeerror=0
35132         end if 
35133       end if 
35134 ! 
35135       if ~writeerror then 
35136         for index=1 to udim(mat controlname$)
35137 ! 
35138           let screenfld$(sf_screencode) = screenio$(si_screencode)
35139           let screenfld$(sf_controlname) = controlname$(index)
35140           let screenfld$(sf_fieldname) = fieldname$(index)
35141           let screenfld$(sf_description) = description$(index)
35142           let screenfld(sf_vposition) = vposition(index)
35143           let screenfld(sf_hposition) = hposition(index)
35144           let screenfld$(sf_fieldtype) = fieldtype$(index)
35145           let screenfld(sf_specwidth) = specwidth(index)
35146           let screenfld(sf_width) = width(index)
35147           let screenfld(sf_height) = height(index)
35148           let screenfld$(sf_truevalue) = truevalue$(index)
35149           let screenfld$(sf_falsevalue) = falsevalue$(index)
35150           let screenfld$(sf_function) = function$(index)
35151           let screenfld$(sf_picture) = picture$(index)
35152           let screenfld$(sf_parent) = parent$(index)
35153           let screenfld$(sf_fgcolor) = fgcolor$(index)
35154           let screenfld$(sf_bgcolor) = bgcolor$(index)
35155           let screenfld$(sf_justify) = justify$(index)
35156           let screenfld$(sf_attr) = attr$(index)
35157           let screenfld(sf_protected) = protected(index)
35158           let screenfld(sf_invisible) = invisible(index)
35159           let screenfld$(sf_tooltip) = tooltip$(index)
35160           let screenfld$(sf_cnvrtin) = cnvrtin$(index)
35161           let screenfld$(sf_cnvrtout) = cnvrtout$(index)
35162           let screenfld(sf_multiselect) = multiselect(index)
35163           let screenfld(sf_gridlines) = gridlines(index)
35164           let screenfld$(sf_userdata) = userdata$(index)
35165 ! 
35166           write #fscreenfld, using form$(fscreenfld) : mat screenfld$, mat screenfld
35167 ! 
35168         next index
35169 ! 
35170         let fncompilehelperlibrary(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,"",waitforcomplete)
35171 ! 
35172         let static_screenname$=screenio$(si_screencode)
35173         let fnpreservescreenarrays(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35174       end if 
35175     else 
35176       let msgbox("Window Name cannot be blank when saving screen.","Error","OK","ERR")
35177       let writeerror=1 !  No Screen Name Passed In
35178     end if 
35179 ! 
35180     let fnwritescreen=~writeerror
35181   fnend 
35182 ! 
35183 CLOSESCREENFILES: !  Closes The Screen Files If They Are Open
35184   def fnclosescreenfiles
35185     if fscreenio<>0 and file(fscreenio)<>-1 then 
35186       let fnclosefile(fscreenio,"screenio","",1)
35187     end if 
35188     if fscreenfld<>0 and file(fscreenfld)<>-1 then 
35189       let fnclosefile(fscreenfld,"screenfld","",1)
35190     end if 
35191   fnend 
35192 ! 
35193 OPENSCREENFILES: ! Open The Screen Files If They Aren't open yet
35194   def fnopenscreenfiles(mat screenio$,mat screenio)
35195     if fscreenio=0 or file(fscreenio)=-1 then 
35196       let fscreenio=fnopen("screenio",mat screenio$, mat screenio, mat form$)
35197       mat tempscreenio$(udim(mat screenio$))
35198       mat tempscreenio(udim(mat screenio))
35199     end if 
35200     if fscreenfld=0 or file(fscreenfld)=-1 then 
35201       let fscreenfld=fnopen("screenfld",mat screenfld$, mat screenfld, mat form$)
35202       mat tempscreenfld$(udim(mat screenfld$))
35203       mat tempscreenfld(udim(mat screenfld))
35204     end if 
35205   fnend 
35206 ! 
35207 DELETESCREEN: ! Deletes The Given Screen
35208   def fndeletescreen(screencode$)
35209     delete #fscreenio, key=fnkey$(fscreenio,screencode$): nokey ignore
35210     let fndeleteallfields(screencode$)
35211   fnend 
35212 ! 
35213 READALLSCREENCODES: ! Read All The Screen Codes From The Screenio File
35214   def fnreadallscreencodes(mat list$,mat create,mat modify)
35215     restore #fscreenio: 
35216     mat list$(0)
35217     mat create(0)
35218     mat modify(0)
35219     do while file(fscreenio)=0
35220       read #fscreenio, using form$(fscreenio), release: mat tempscreenio$, mat tempscreenio eof ignore
35221       if file(fscreenio)=0 then 
35222         mat list$(udim(mat list$)+1)
35223         mat create(udim(mat list$))
35224         mat modify(udim(mat list$))
35225         let list$(udim(mat list$))=tempscreenio$(si_screencode)
35226         let create(udim(mat list$))=tempscreenio(si_createdate)
35227         let modify(udim(mat list$))=tempscreenio(si_modifydate)
35228       end if 
35229     loop 
35230   fnend 
35231 ! 
35232 DELETEALLFIELDS: ! Delete All  The Controls For The Associated Screen So They Can Be Resaved
35233   def fndeleteallfields(screenname$)
35234     restore #fscreenfld,key=fnkey$(fscreenfld,screenname$): nokey ignore
35235     do while file(fscreenfld)=0
35236       read #fscreenfld, using form$(fscreenfld) : mat tempscreenfld$, mat tempscreenfld eof ignore
35237       if file(fscreenfld)=0 and trim$(tempscreenfld$(sf_screencode))=trim$(screenname$) then 
35238         delete #fscreenfld: 
35239       end if 
35240     loop while trim$(tempscreenfld$(sf_screencode))=trim$(screenname$)
35241   fnend 
35242 ! 
35243 RESIZECONTROLARRAYS: !  Resize The Given Arrays  To The Given Size
35244   def fnresizecontrolarrays(newsize, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35245     mat controlname$(newsize)
35246     mat fieldname$(newsize)
35247     mat description$(newsize)
35248     mat vposition(newsize)
35249     mat hposition(newsize)
35250     mat fieldtype$(newsize)
35251     mat specwidth(newsize)
35252     mat width(newsize)
35253     mat height(newsize)
35254     mat truevalue$(newsize)
35255     mat falsevalue$(newsize)
35256     mat function$(newsize)
35257     mat picture$(newsize)
35258     mat parent$(newsize)
35259     mat fgcolor$(newsize)
35260     mat bgcolor$(newsize)
35261     mat justify$(newsize)
35262     mat attr$(newsize)
35263     mat protected(newsize)
35264     mat invisible(newsize)
35265     mat tooltip$(newsize)
35266     mat cnvrtin$(newsize)
35267     mat cnvrtout$(newsize)
35268     mat multiselect(newsize)
35269     mat gridlines(newsize)
35270     mat userdata$(newsize)
35271   fnend 
35272 ! 
35273 COPYCONTROLARRAYS: ! Copies All The Values From One Control To Another
35274   def fncopycontrolarrays(to,from,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35275     let controlname$(to)=controlname$(from)
35276     let fieldname$(to)=fieldname$(from)
35277     let description$(to)=description$(from)
35278     let vposition(to)=vposition(from)
35279     let hposition(to)=hposition(from)
35280     let fieldtype$(to)=fieldtype$(from)
35281     let specwidth(to)=specwidth(from)
35282     let width(to)=width(from)
35283     let height(to)=height(from)
35284     let truevalue$(to)=truevalue$(from)
35285     let falsevalue$(to)=falsevalue$(from)
35286     let function$(to)=function$(from)
35287     let picture$(to)=picture$(from)
35288     let parent$(to)=parent$(from)
35289     let fgcolor$(to)=fgcolor$(from)
35290     let bgcolor$(to)=bgcolor$(from)
35291     let justify$(to)=justify$(from)
35292     let attr$(to)=attr$(from)
35293     let protected(to)=protected(from)
35294     let invisible(to)=invisible(from)
35295     let tooltip$(to)=tooltip$(from)
35296     let cnvrtin$(to)=cnvrtin$(from)
35297     let cnvrtout$(to)=cnvrtout$(from)
35298     let multiselect(to)=multiselect(from)
35299     let gridlines(to)=gridlines(from)
35300     let userdata$(to)=userdata$(from)
35301   fnend 
35302 ! 
35303 SWAPCONTROLARRAYS: ! Swaps Two Control Arrays
35304   def fnswapcontrolarrays(to,from,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,controlname$*50,fieldname$*50,description$*255,vposition,hposition,fieldtype$,specwidth,width,height,truevalue$*60,falsevalue$*60,function$*255,picture$*255,parent$*20,fgcolor$,bgcolor$,protected,invisible,tooltip$*255,cnvrtin$*255,cnvrtout$*255,multiselect,userdata$*255,gridlines,justify$,attr$*255)
35305     let controlname$=controlname$(to)
35306     let fieldname$=fieldname$(to)
35307     let description$=description$(to)
35308     let vposition=vposition(to)
35309     let hposition=hposition(to)
35310     let fieldtype$=fieldtype$(to)
35311     let specwidth=specwidth(to)
35312     let width=width(to)
35313     let height=height(to)
35314     let truevalue$=truevalue$(to)
35315     let falsevalue$=falsevalue$(to)
35316     let function$=function$(to)
35317     let picture$=picture$(to)
35318     let parent$=parent$(to)
35319     let fgcolor$=fgcolor$(to)
35320     let bgcolor$=bgcolor$(to)
35321     let justify$=justify$(to)
35322     let attr$=attr$(to)
35323     let protected=protected(to)
35324     let invisible=invisible(to)
35325     let tooltip$=tooltip$(to)
35326     let cnvrtin$=cnvrtin$(to)
35327     let cnvrtout$=cnvrtout$(to)
35328     let multiselect=multiselect(to)
35329     let gridlines=gridlines(to)
35330     let userdata$=userdata$(to)
35331 ! 
35332     let controlname$(to)=controlname$(from)
35333     let fieldname$(to)=fieldname$(from)
35334     let description$(to)=description$(from)
35335     let vposition(to)=vposition(from)
35336     let hposition(to)=hposition(from)
35337     let fieldtype$(to)=fieldtype$(from)
35338     let specwidth(to)=specwidth(from)
35339     let width(to)=width(from)
35340     let height(to)=height(from)
35341     let truevalue$(to)=truevalue$(from)
35342     let falsevalue$(to)=falsevalue$(from)
35343     let function$(to)=function$(from)
35344     let picture$(to)=picture$(from)
35345     let parent$(to)=parent$(from)
35346     let fgcolor$(to)=fgcolor$(from)
35347     let bgcolor$(to)=bgcolor$(from)
35348     let justify$(to)=justify$(from)
35349     let attr$(to)=attr$(from)
35350     let protected(to)=protected(from)
35351     let invisible(to)=invisible(from)
35352     let tooltip$(to)=tooltip$(from)
35353     let cnvrtin$(to)=cnvrtin$(from)
35354     let cnvrtout$(to)=cnvrtout$(from)
35355     let multiselect(to)=multiselect(from)
35356     let gridlines(to)=gridlines(from)
35357     let userdata$(to)=userdata$(from)
35358 ! 
35359     let controlname$(from)=controlname$
35360     let fieldname$(from)=fieldname$
35361     let description$(from)=description$
35362     let vposition(from)=vposition
35363     let hposition(from)=hposition
35364     let fieldtype$(from)=fieldtype$
35365     let specwidth(from)=specwidth
35366     let width(from)=width
35367     let height(from)=height
35368     let truevalue$(from)=truevalue$
35369     let falsevalue$(from)=falsevalue$
35370     let function$(from)=function$
35371     let picture$(from)=picture$
35372     let parent$(from)=parent$
35373     let fgcolor$(from)=fgcolor$
35374     let bgcolor$(from)=bgcolor$
35375     let justify$(from)=justify$
35376     let attr$(from)=attr$
35377     let protected(from)=protected
35378     let invisible(from)=invisible
35379     let tooltip$(from)=tooltip$
35380     let cnvrtin$(from)=cnvrtin$
35381     let cnvrtout$(from)=cnvrtout$
35382     let multiselect(from)=multiselect
35383     let gridlines(from)=gridlines
35384     let userdata$(from)=userdata$
35385   fnend 
35386 ! 
35387 EXPORTSCREEN: ! Exports The Screen To A User Selected File
35388   def fnexportscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,fexportfile,index,csopenstring$)
35389     open #(fexportfile:=fngetfilenumber): "name=save:"&csopenstring$&"*.sio,new,recl="&str$(max(rln(fscreenio),rln(fscreenfld))),internal,output,sequential error ignore
35390     if file(fexportfile)=0 then 
35391       let fnwriteexport(fexportfile,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35392 ! 
35393       close #fexportfile: 
35394       let fnexportscreen=1
35395     end if 
35396   fnend 
35397 ! 
35398   def fnwritebackup(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,filename$*255,index)
35399     if ~exists("screen\backup") then 
35400       execute "mkdir screen\backup" error ignore
35401     end if 
35402 ! 
35403     let filename$=uprc$(trim$(screenio$(si_screencode)))
35404     let index=0
35405     do while (index+=1)<=len(filename$)
35406       if ~pos("ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",filename$(index:index)) then 
35407         let filename$(index:index)=""
35408         let index-=1
35409       end if 
35410     loop 
35411 ! 
35412     open #(backupfile:=fngetfilenumber): "name=screen\backup\"&lwrc$(filename$)&".sio,replace,recl="&str$(max(rln(fscreenio),rln(fscreenfld))),internal,output,sequential error ignore
35413     if file(backupfile)=0 then 
35414       let fnwriteexport(backupfile,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35415       close #backupfile: error ignore
35416       let fnwritebackup=1
35417     end if 
35418   fnend 
35419 ! 
35420   def fnwriteexport(fexportfile,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
35421     write #fexportfile, using form$(fscreenio) : mat screenio$, mat screenio
35422     for index=1 to udim(mat controlname$)
35423       let screenfld$(sf_screencode) = screenio$(si_screencode)
35424       let screenfld$(sf_controlname) = controlname$(index)
35425       let screenfld$(sf_fieldname) = fieldname$(index)
35426       let screenfld$(sf_description) = description$(index)
35427       let screenfld(sf_vposition) = vposition(index)
35428       let screenfld(sf_hposition) = hposition(index)
35429       let screenfld$(sf_fieldtype) = fieldtype$(index)
35430       let screenfld(sf_specwidth) = specwidth(index)
35431       let screenfld(sf_width) = width(index)
35432       let screenfld(sf_height) = height(index)
35433       let screenfld$(sf_truevalue) = truevalue$(index)
35434       let screenfld$(sf_falsevalue) = falsevalue$(index)
35435       let screenfld$(sf_function) = function$(index)
35436       let screenfld$(sf_picture) = picture$(index)
35437       let screenfld$(sf_parent) = parent$(index)
35438       let screenfld$(sf_fgcolor) = fgcolor$(index)
35439       let screenfld$(sf_bgcolor) = bgcolor$(index)
35440       let screenfld$(sf_justify) = justify$(index)
35441       let screenfld$(sf_attr) = attr$(index)
35442       let screenfld(sf_protected) = protected(index)
35443       let screenfld(sf_invisible) = invisible(index)
35444       let screenfld$(sf_tooltip) = tooltip$(index)
35445       let screenfld$(sf_cnvrtin) = cnvrtin$(index)
35446       let screenfld$(sf_cnvrtout) = cnvrtout$(index)
35447       let screenfld(sf_multiselect) = multiselect(index)
35448       let screenfld(sf_gridlines) = gridlines(index)
35449       let screenfld$(sf_userdata) = userdata$(index)
35450 ! .!
35451       write #fexportfile, using form$(fscreenfld) : mat screenfld$, mat screenfld
35452     next index
35453   fnend 
35454 ! 
35455 IMPORTSCREEN: ! Imports The Screen From A User Selected File
35456   def fnimportscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,fimportfile,index,csopenstring$)
35457 ! .   ! if Fnclientserver then let Csopenstring$="@:"
35458     open #(fimportfile:=fngetfilenumber): "name=open:"&csopenstring$&"*.sio",internal,input,sequential error ignore
35459     if file(fimportfile)=0 then 
35460 ! 
35461       read #fimportfile, using form$(fscreenio), release: mat screenio$, mat screenio error ignore
35462 ! 
35463       let fnresizecontrolarrays(0, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35464 ! 
35465       do while file(fimportfile)=0
35466         read #fimportfile, using form$(fscreenfld), release: mat screenfld$, mat screenfld eof ignore
35467         if file(fimportfile)=0 then 
35468           let index=udim(mat controlname$)+1
35469           let fnresizecontrolarrays(index, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35470 ! 
35471           let controlname$(index)=screenfld$(sf_controlname)
35472           let fieldname$(index)=screenfld$(sf_fieldname)
35473           let description$(index)=screenfld$(sf_description)
35474           let vposition(index)=screenfld(sf_vposition)
35475           let hposition(index)=screenfld(sf_hposition)
35476           let fieldtype$(index)=screenfld$(sf_fieldtype)
35477           let specwidth(index)=screenfld(sf_specwidth)
35478           let width(index)=screenfld(sf_width)
35479           let height(index)=screenfld(sf_height)
35480           let truevalue$(index)=screenfld$(sf_truevalue)
35481           let falsevalue$(index)=screenfld$(sf_falsevalue)
35482           let function$(index)=screenfld$(sf_function)
35483           let picture$(index)=screenfld$(sf_picture)
35484           let parent$(index)=screenfld$(sf_parent)
35485           let fgcolor$(index)=screenfld$(sf_fgcolor)
35486           let bgcolor$(index)=screenfld$(sf_bgcolor)
35487           let justify$(index)=screenfld$(sf_justify)
35488           let attr$(index)=screenfld$(sf_attr)
35489           let protected(index)=screenfld(sf_protected)
35490           let invisible(index)=screenfld(sf_invisible)
35491           let tooltip$(index)=screenfld$(sf_tooltip)
35492           let cnvrtin$(index)=screenfld$(sf_cnvrtin)
35493           let cnvrtout$(index)=screenfld$(sf_cnvrtout)
35494           let multiselect(index)=screenfld(sf_multiselect)
35495           let gridlines(index)=screenfld(sf_gridlines)
35496           let userdata$(index)=screenfld$(sf_userdata)
35497         end if 
35498       loop 
35499 ! 
35500       close #fimportfile: 
35501       let fnimportscreen=1
35502     end if 
35503   fnend 
35504 ! 
35505 RECOMPILEALLSCREENS: ! Recompiles Every Screen
35506   def fnrecompileallscreens(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,savescreencode$,index,time,filename$)
35507     dim tempscreenlist$(1),tempcreatedate(1),tempmodifydate(1)
35508     dim failedlist$*2047
35509     let savescreencode$=static_screenname$
35510     let fnreadallscreencodes(mat tempscreenlist$,mat tempcreatedate,mat tempmodifydate)
35511 ! 
35512     if exists("compile[SESSION].$$$") then 
35513       execute "free compile[SESSION].$$$"
35514     end if 
35515 ! 
35516     let failedlist$=""
35517     for index=1 to udim(mat tempscreenlist$)
35518       if fnreadscreen(tempscreenlist$(index),mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
35519         let fncompilehelperlibrary(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,filename$)
35520         let fnwaitforfile(failedlist$,lwrc$(trim$(tempscreenlist$(index))),filename$)
35521       end if 
35522     next index
35523 ! 
35524     if len(failedlist$) then 
35525       let msgbox("The following screens took a long time and may have had errors:"&chr$(13)&chr$(13)&failedlist$(3:9999))
35526     end if 
35527 ! 
35528     let fnreadscreen(savescreencode$,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
35529   fnend 
35530 ! 
35531   def fnwaitforfile(&failedlist$,screen$;&filename$,___,time,window,ky$,try,x)
35532     if filename$="" then let filename$="compile"&session$&".$$$"
35533     do while exists(filename$)
35534       let sleep(.1)
35535       let time+=1
35536       if time>20 then 
35537         let failedlist$=failedlist$&", "&screen$
35538         open #(window:=fngetfilenumber): "srow=14,scol=45,rows=3,cols=30,n=/#FFFFFF:#FF0000", display, outin 
35539         print #window, fields "1,1,90/C 120,S" : "We're sorry. The compile process appears to be stuck. Close the open window or press the Space Bar to continue."
35540         do while kstat$<>"" ! Clear Keyboard Buffer
35541         loop 
35542         do while exists(filename$)
35543           let sleep(.1)
35544           let ky$=kstat$
35545           if ky$=" " then 
35546             let try+=1
35547             if try=1 then 
35548               execute "free "&filename$ error ignore
35549               if exists(filename$) then 
35550                 print #window, fields "1,1,90/C 120,S" : "The file is locked. Please close any BR windows or press Space again to force it."
35551               end if 
35552             else if try=2 then 
35553               do while exists(filename$)
35554                 let x+=1
35555                 let filename$="compile"&session$&"-"&str$(x)&".$$$"
35556               loop 
35557             end if 
35558           end if 
35559         loop 
35560         close #window: 
35561       end if 
35562     loop 
35563   fnend 
35564 ! 
35565 COMPILEHELPERLIBRARY: ! Compiles The Helper Library For The Screen
35566   def fncompilehelperlibrary(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;filename$,waitforcomplete,___,dummy$*255,index,filenumber,elseif$,library$*80,funccall$*255,longstring$*800,function$*255,longstring2$*800,debugscreen$,pathinfo$*511,debugext$)
35567 ! 
35568     if filename$="" then let filename$="compile"&session$&".$$$"
35569 ! 
35570     dim alreadysetfunctions$(1)*255
35571     mat alreadysetfunctions$(0)
35572     dim newincludefunctions$(1)*255
35573     mat newincludefunctions$(0)
35574 ! 
35575     let fncheckhelperlibfolder
35576 ! .   ! Create new display file or replace it
35577     open #(filenumber:=fngetfilenumber) : "name="&setting_screenfolder$&"\"&trim$(screenio$(si_screencode))&".brs, recl=800, replace",display,output 
35578 ! 
35579 ! .   ! Print heading to it
35580     let fnsetlinenumber(10,10,filenumber)
35581     let fnprintline("! "&setting_screenfolder$&"\"&lwrc$(trim$(screenio$(si_screencode)))&".br")
35582     let fnprintline("! ScreenIO Generated Helper Library for "&uprc$(trim$(screenio$(si_screencode)))&" screen")
35583     let fnprintline("! Copyright 2008 by Sage AX")
35584     let fnprintline("! ")
35585     let fnprintline("! Compiled On: "&date$("mm/dd/ccyy"))
35586     let fnprintline("! ")
35587     let fnprintline('! ')
35588     let fnprintline('! ')
35589 ! 
35590 ! .   ! Dim Helper Library Variables
35591     let fnsetlinenumber(300,10)
35592     let fnprintline('! ')
35593     let fnprintline('   option retain')
35594     let fnprintline('   dim FileIOLinkageSet')
35595     let fnprintline('   dim Ret$*255')
35596     let fnprintline('   dim PassedData$(1)*255')
35597     let fnunpackpdata$(mat pdata$,screenio$(si_debugpassed))
35598     let fnprintline('   mat PassedData$('&str$(udim(mat pdata$))&')')
35599     let fnsetlinenumber(400,1)
35600     for index=1 to udim(mat pdata$)
35601       let fnprintline('   let PassedData$('&str$(index)&')="'&srep$(pdata$(index),"""","""""")&'"')
35602     next index
35603 ! 
35604     let fnsetlinenumber(600,10)
35605     if len(trim$(screenio$(si_debugpath))) then 
35606       let fnprintline('   dim MyF$(1)*255')
35607       let fnprintline('   dim MyF(1)')
35608       let pathinfo$=",0,mat MyF$,mat MyF,"""&trim$(screenio$(si_debugpath))&""""
35609     end if 
35610 ! 
35611     let fnprintline('! ')
35612 ! 
35613 ! .   ! Print Main Routine (Links to screen, runs it)
35614     let fnsetlinenumber(1000,10)
35615     let fnprintline('Main: ! If you run me as a program, I run the '&lwrc$(trim$(screenio$(si_screencode)))& ' screen')
35616     let fnprintline('   library "'&setting_screeniopath$&'" : fnfm$')
35617 ! 
35618     if len(trim$(screenio$(si_debugscreen))) then 
35619       let debugscreen$=lwrc$(trim$(screenio$(si_debugscreen)))
35620     else 
35621       let debugscreen$=lwrc$(trim$(screenio$(si_screencode)))
35622     end if 
35623 ! 
35624     if debugscreen$(1:1)="%" then 
35625       let fnprintline('   let Key$="'&srep$(screenio$(si_debugkey),"""","""""")&'"')
35626       let fnprintline('   let ParentKey$="'&srep$(screenio$(si_debugparentkey),"""","""""")&'"')
35627       let fnprintline('   let Record='&str$(screenio(si_debugrecord)))
35628       let fnprintline('   let Path$="'&srep$(screenio$(si_debugpath),"""","""""")&'"')
35629       if pos(lwrc$(debugscreen$),".br") or pos(lwrc$(debugscreen$),".wb") then 
35630         let fnprintline('   chain "'&debugscreen$(2:99)&'",mat PassedData$,Key$,ParentKey$,Record,Path$')
35631       else 
35632         let fnprintline('   chain "proc='&debugscreen$(2:99)&'",mat PassedData$,Key$,ParentKey$,Record,Path$')
35633       end if 
35634     else 
35635       let fnprintline('   let Ret$=fnfm$("'&debugscreen$&'","'&trim$(screenio$(si_debugkey))&'",0,0,"'&trim$(screenio$(si_debugparentkey))&'",0,0,0,'&str$(screenio(si_debugrecord))&',mat PassedData$'&pathinfo$&')')
35636     end if 
35637 ! 
35638     let fnprintline('   if len(trim$(Ret$)) then')
35639     let fnprintline('      print Ret$')
35640     let fnprintline('   end if')
35641     let fnprintline('   if fkey=93 then execute "system"')
35642     let fnprintline('   stop')
35643     let fnprintline('! ')
35644 ! 
35645 ! .   ! Print Custom Functions
35646     let fnsetlinenumber(5000,1)
35647     let fnprintline('CustomFunctions: ! Lines 5000 - 70000 are Custom Functions')
35648     let fnprintline('! '&rpt$("=",60))
35649     for index=1 to udim(mat function$)
35650       let function$=trim$(function$(index))
35651       if function$(1:1)="[" and pos(function$,"]") then 
35652         let function$=trim$(function$(pos(function$,"]")+1:len(function$)))
35653       end if 
35654       if function$(1:1)="{" then ! We Have An Internal Function
35655         if srch(alreadysetfunctions$,lwrc$(trim$(function$)))<=0 then 
35656           if exists(fncustomfilenameof$(function$)) then 
35657             let fnimportfunction(function$,mat newincludefunctions$)
35658             let fnprintline('! ')
35659           else 
35660             let msgbox("Missing Function: "&function$,"Where did it go?","OK","ERR")
35661           end if 
35662           mat alreadysetfunctions$(udim(mat alreadysetfunctions$)+1)
35663           let alreadysetfunctions$(udim(mat alreadysetfunctions$))=lwrc$(trim$(function$))
35664         end if 
35665       end if 
35666     next index
35667     for index=si_enterfn to si_exitfn
35668       let function$=trim$(screenio$(index))
35669       if function$(1:1)="[" and pos(function$,"]") then 
35670         let function$=trim$(function$(pos(function$,"]")+1:len(function$)))
35671       end if 
35672       if function$(1:1)="{" then ! We Have An Internal Function
35673         if srch(alreadysetfunctions$,lwrc$(trim$(function$)))<=0 then 
35674           if exists(fncustomfilenameof$(function$)) then 
35675             let fnimportfunction(function$,mat newincludefunctions$)
35676             let fnprintline('! ')
35677           else 
35678             let msgbox("Missing Function: "&function$,"Where did it go?","OK","ERR")
35679           end if 
35680           mat alreadysetfunctions$(udim(mat alreadysetfunctions$)+1)
35681           let alreadysetfunctions$(udim(mat alreadysetfunctions$))=lwrc$(trim$(function$))
35682         end if 
35683       end if 
35684     next index
35685 ! 
35686     if exists(setting_functionfolder$) then 
35687       if exists(setting_functionfolder$&"defaults\") then 
35688         if exists(setting_functionfolder$&"defaults\enter.brs") then 
35689           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\enter.brs"),mat newincludefunctions$)
35690         end if 
35691         if exists(setting_functionfolder$&"defaults\init.brs") then 
35692           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\init.brs"),mat newincludefunctions$)
35693         end if 
35694         if exists(setting_functionfolder$&"defaults\read.brs") then 
35695           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\read.brs"),mat newincludefunctions$)
35696         end if 
35697         if exists(setting_functionfolder$&"defaults\load.brs") then 
35698           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\load.brs"),mat newincludefunctions$)
35699         end if 
35700         if exists(setting_functionfolder$&"defaults\write.brs") then 
35701           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\write.brs"),mat newincludefunctions$)
35702         end if 
35703         if exists(setting_functionfolder$&"defaults\wait.brs") then 
35704           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\wait.brs"),mat newincludefunctions$)
35705         end if 
35706         if exists(setting_functionfolder$&"defaults\locked.brs") then 
35707           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\locked.brs"),mat newincludefunctions$)
35708         end if 
35709         if exists(setting_functionfolder$&"defaults\merge.brs") then 
35710           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\merge.brs"),mat newincludefunctions$)
35711         end if 
35712         if exists(setting_functionfolder$&"defaults\mainloop.brs") then 
35713           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\mainloop.brs"),mat newincludefunctions$)
35714         end if 
35715         if exists(setting_functionfolder$&"defaults\nokey.brs") then 
35716           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\nokey.brs"),mat newincludefunctions$)
35717         end if 
35718         if exists(setting_functionfolder$&"defaults\prelist.brs") then 
35719           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\prelist.brs"),mat newincludefunctions$)
35720         end if 
35721         if exists(setting_functionfolder$&"defaults\postlist.brs") then 
35722           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\postlist.brs"),mat newincludefunctions$)
35723         end if 
35724         if exists(setting_functionfolder$&"defaults\exit.brs") then 
35725           let fnimportfunction(fnfunctionstring$(setting_functionfolder$&"defaults\exit.brs"),mat newincludefunctions$)
35726         end if 
35727       end if 
35728     end if 
35729 ! 
35730     let index=0
35731     do while index<udim(mat newincludefunctions$)
35732       let index+=1
35733       let function$=newincludefunctions$(index)
35734       if function$(1:1)="{" then ! We Have An Internal Function
35735         if srch(alreadysetfunctions$,lwrc$(trim$(function$)))<=0 then 
35736           if exists(fncustomfilenameof$(function$)) then 
35737             let fnimportfunction(function$,mat newincludefunctions$)
35738             let fnprintline('! ')
35739           else 
35740             let msgbox("Missing Function: "&function$,"Where did it go?","OK","ERR")
35741           end if 
35742           mat alreadysetfunctions$(udim(mat alreadysetfunctions$)+1)
35743           let alreadysetfunctions$(udim(mat alreadysetfunctions$))=lwrc$(trim$(function$))
35744         end if 
35745       end if 
35746     loop 
35747 ! 
35748     let fnprintline('! ')
35749     let fnprintline('! ')
35750 ! 
35751 ! .   ! Generate String Reference Chart Function
35752     let fnsetlinenumber(85000,1)
35753     let fnprintline('   def library fnCheckStringFunction(Function$*255)')
35754     let elseif$="if "
35755     for index=1 to udim(mat function$)
35756       let function$=trim$(function$(index))
35757       if function$(1:1)="{" then ! We Have A Custom Function
35758         let funccall$=fnreadcustomfunctionstatement$(function$)
35759         if len(trim$(funccall$)) then 
35760           let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
35761           let fnprintline('      let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35762           let elseif$="else if "
35763         end if 
35764       end if 
35765     next index
35766     for index=si_enterfn to si_exitfn
35767       let function$=trim$(screenio$(index))
35768       if function$(1:1)="{" then ! We Have A Custom Function
35769         let funccall$=fnreadcustomfunctionstatement$(function$)
35770         if len(trim$(funccall$)) then 
35771           let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35772           let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35773           let elseif$="else if "
35774         end if 
35775       end if 
35776     next index
35777 ! 
35778     if exists(setting_functionfolder$) then 
35779       if exists(setting_functionfolder$&"defaults\") then 
35780         if exists(setting_functionfolder$&"defaults\enter.brs") then 
35781           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\enter.brs")
35782           let funccall$=fnreadcustomfunctionstatement$(function$)
35783           if len(trim$(funccall$)) then 
35784             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35785             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35786             let elseif$="else if "
35787           end if 
35788         end if 
35789         if exists(setting_functionfolder$&"defaults\init.brs") then 
35790           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\init.brs")
35791           let funccall$=fnreadcustomfunctionstatement$(function$)
35792           if len(trim$(funccall$)) then 
35793             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35794             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35795             let elseif$="else if "
35796           end if 
35797         end if 
35798         if exists(setting_functionfolder$&"defaults\read.brs") then 
35799           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\read.brs")
35800           let funccall$=fnreadcustomfunctionstatement$(function$)
35801           if len(trim$(funccall$)) then 
35802             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35803             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35804             let elseif$="else if "
35805           end if 
35806         end if 
35807         if exists(setting_functionfolder$&"defaults\load.brs") then 
35808           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\load.brs")
35809           let funccall$=fnreadcustomfunctionstatement$(function$)
35810           if len(trim$(funccall$)) then 
35811             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35812             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35813             let elseif$="else if "
35814           end if 
35815         end if 
35816         if exists(setting_functionfolder$&"defaults\write.brs") then 
35817           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\write.brs")
35818           let funccall$=fnreadcustomfunctionstatement$(function$)
35819           if len(trim$(funccall$)) then 
35820             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35821             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35822             let elseif$="else if "
35823           end if 
35824         end if 
35825         if exists(setting_functionfolder$&"defaults\wait.brs") then 
35826           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\wait.brs")
35827           let funccall$=fnreadcustomfunctionstatement$(function$)
35828           if len(trim$(funccall$)) then 
35829             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35830             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35831             let elseif$="else if "
35832           end if 
35833         end if 
35834         if exists(setting_functionfolder$&"defaults\locked.brs") then 
35835           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\locked.brs")
35836           let funccall$=fnreadcustomfunctionstatement$(function$)
35837           if len(trim$(funccall$)) then 
35838             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35839             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35840             let elseif$="else if "
35841           end if 
35842         end if 
35843         if exists(setting_functionfolder$&"defaults\merge.brs") then 
35844           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\merge.brs")
35845           let funccall$=fnreadcustomfunctionstatement$(function$)
35846           if len(trim$(funccall$)) then 
35847             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35848             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35849             let elseif$="else if "
35850           end if 
35851         end if 
35852         if exists(setting_functionfolder$&"defaults\mainloop.brs") then 
35853           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\mainloop.brs")
35854           let funccall$=fnreadcustomfunctionstatement$(function$)
35855           if len(trim$(funccall$)) then 
35856             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35857             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35858             let elseif$="else if "
35859           end if 
35860         end if 
35861         if exists(setting_functionfolder$&"defaults\nokey.brs") then 
35862           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\nokey.brs")
35863           let funccall$=fnreadcustomfunctionstatement$(function$)
35864           if len(trim$(funccall$)) then 
35865             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35866             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35867             let elseif$="else if "
35868           end if 
35869         end if 
35870         if exists(setting_functionfolder$&"defaults\prelist.brs") then 
35871           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\prelist.brs")
35872           let funccall$=fnreadcustomfunctionstatement$(function$)
35873           if len(trim$(funccall$)) then 
35874             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35875             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35876             let elseif$="else if "
35877           end if 
35878         end if 
35879         if exists(setting_functionfolder$&"defaults\postlist.brs") then 
35880           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\postlist.brs")
35881           let funccall$=fnreadcustomfunctionstatement$(function$)
35882           if len(trim$(funccall$)) then 
35883             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35884             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35885             let elseif$="else if "
35886           end if 
35887         end if 
35888         if exists(setting_functionfolder$&"defaults\exit.brs") then 
35889           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\exit.brs")
35890           let funccall$=fnreadcustomfunctionstatement$(function$)
35891           if len(trim$(funccall$)) then 
35892             let fnprintline('      '&elseif$&'Function$ = "'&function$&'" then')
35893             let fnprintline('         let fnCheckStringFunction = '&str$(fnstringfunction(funccall$)))
35894             let elseif$="else if "
35895           end if 
35896         end if 
35897       end if 
35898     end if 
35899 ! 
35900     if elseif$="else if " then 
35901       let fnprintline('      else')
35902       let fnprintline('         if Function$<>"{{GetData}}" and Function$<>"{{SetData}}" then')
35903       let fnprintline('            print "Function ("&function$&") Not Supported: The library is out of date or fn not found."')
35904       let fnprintline('         end if')
35905       let fnprintline('      end if')
35906     else 
35907       let fnprintline('      if Function$<>"{{GetData}}" and Function$<>"{{SetData}}" then')
35908       let fnprintline('         print "Function ("&function$&") Not Supported: The library is out of date or fn not found."')
35909       let fnprintline('      end if')
35910     end if 
35911 ! 
35912     let fnprintline('   fnend')
35913     let fnprintline('! ')
35914     let fnprintline('! ')
35915 ! 
35916 ! .   ! Print Function Switch
35917     let fnsetlinenumber(89000,10)
35918     let longstring$='Function$*255,mat Subscripts$,mat PassedData$,&FieldIndex,&FieldText$,&ControlIndex,&Key$,&ExitMode,&RepopulateListviews,&RedrawListviews,&CurrentField,&CurrentField$,&Prefix$,&CurrentKey$,&CurrentRec,&ParentKey$,'
35919     let longstring$=longstring$&'&DataFile,Window,&CurrentRow,LockUser$,&Path$,&Selecting,mat Disk_F$,mat Disk_F,&OnRead,&TryToContinue,&DisplayOnly,&Active,&RedrawFrames,&RedrawScreens,&RepopulateCombo,mat F$,mat F, mat S$,mat CnvrtIn$,mat FieldsSSubs$,'
35920     let longstring$=longstring$&'mat FieldsNSubs$,mat FieldName$,mat ControlName$,mat ScreenSubs$,mat Function$,mat SpecWidth,mat Form$,mat ScreenData$,mat ScreenLongData$,mat ScreenData,mat ReturnData$,mat ControlSpec$'
35921     let longstring2$='Function$,mat Subscripts$,mat PassedData$,FieldIndex,FieldText$,ControlIndex,Key$,ExitMode,RepopulateListviews,RedrawListviews,CurrentField,CurrentField$,Prefix$,CurrentKey$,CurrentRec,ParentKey$,'
35922     let longstring2$=longstring2$&'DataFile,Window,CurrentRow,LockUser$,Path$,Selecting,mat Disk_F$,mat Disk_F,OnRead,TryToContinue,DisplayOnly,Active,RedrawFrames,RedrawScreens,RepopulateCombo,mat F$,mat F, mat S$,mat CnvrtIn$,mat FieldsSSubs$,'
35923     let longstring2$=longstring2$&'mat FieldsNSubs$,mat FieldName$,mat ControlName$,mat ScreenSubs$,mat Function$,mat SpecWidth,mat Form$,mat ScreenData$,mat ScreenLongData$,mat ScreenData,mat ReturnData$,mat ControlSpec$)'
35924 ! 
35925 ! 
35926     let fnprintline('   def library fnFunctionSwitch('&longstring$&')')
35927     let fnprintline('      let fnFunctionSwitch = fnFS('&longstring2$)
35928     let fnprintline('   fnend')
35929     let fnprintline('   def library fnFunctionSwitch$*255('&longstring$&')')
35930     let fnprintline('      let fnFunctionSwitch$ = fnFS$('&longstring2$)
35931     let fnprintline('   fnend')
35932     let fnprintline('   def library fnFunctionSwitch1('&longstring$&')')
35933     let fnprintline('      let fnFunctionSwitch1 = fnFS('&longstring2$)
35934     let fnprintline('   fnend')
35935     let fnprintline('   def library fnFunctionSwitch1$*255('&longstring$&')')
35936     let fnprintline('      let fnFunctionSwitch1$ = fnFS$('&longstring2$)
35937     let fnprintline('   fnend')
35938     let fnprintline('   def library fnFunctionSwitch2('&longstring$&')')
35939     let fnprintline('      let fnFunctionSwitch2 = fnFS('&longstring2$)
35940     let fnprintline('   fnend')
35941     let fnprintline('   def library fnFunctionSwitch2$*255('&longstring$&')')
35942     let fnprintline('      let fnFunctionSwitch2$ = fnFS$('&longstring2$)
35943     let fnprintline('   fnend')
35944     let fnprintline('   def library fnFunctionSwitch3('&longstring$&')')
35945     let fnprintline('      let fnFunctionSwitch3 = fnFS('&longstring2$)
35946     let fnprintline('   fnend')
35947     let fnprintline('   def library fnFunctionSwitch3$*255('&longstring$&')')
35948     let fnprintline('      let fnFunctionSwitch3$ = fnFS$('&longstring2$)
35949     let fnprintline('   fnend')
35950     let fnprintline('   def library fnFunctionSwitch4('&longstring$&')')
35951     let fnprintline('      let fnFunctionSwitch4 = fnFS('&longstring2$)
35952     let fnprintline('   fnend')
35953     let fnprintline('   def library fnFunctionSwitch4$*255('&longstring$&')')
35954     let fnprintline('      let fnFunctionSwitch4$ = fnFS$('&longstring2$)
35955     let fnprintline('   fnend')
35956     let fnprintline('   def library fnFunctionSwitch5('&longstring$&')')
35957     let fnprintline('      let fnFunctionSwitch5 = fnFS('&longstring2$)
35958     let fnprintline('   fnend')
35959     let fnprintline('   def library fnFunctionSwitch5$*255('&longstring$&')')
35960     let fnprintline('      let fnFunctionSwitch5$ = fnFS$('&longstring2$)
35961     let fnprintline('   fnend')
35962     let fnprintline('   def library fnFunctionSwitch6('&longstring$&')')
35963     let fnprintline('      let fnFunctionSwitch6 = fnFS('&longstring2$)
35964     let fnprintline('   fnend')
35965     let fnprintline('   def library fnFunctionSwitch6$*255('&longstring$&')')
35966     let fnprintline('      let fnFunctionSwitch6$ = fnFS$('&longstring2$)
35967     let fnprintline('   fnend')
35968     let fnprintline('   def library fnFunctionSwitch7('&longstring$&')')
35969     let fnprintline('      let fnFunctionSwitch7 = fnFS('&longstring2$)
35970     let fnprintline('   fnend')
35971     let fnprintline('   def library fnFunctionSwitch7$*255('&longstring$&')')
35972     let fnprintline('      let fnFunctionSwitch7$ = fnFS$('&longstring2$)
35973     let fnprintline('   fnend')
35974 ! 
35975     let fnprintline('! ')
35976     let fnprintline(' dim DataIsInside')
35977     let fnprintline('! ')
35978 ! 
35979     let fnprintline('   def fnFS('&longstring$&';___,ReturnValue,ReturnValue$*255,Index)')
35980     let fnprintline('      gosub FunctionSwitch')
35981     let fnprintline('      let fnFS=ReturnValue')
35982     let fnprintline('   fnend')
35983     let fnprintline('   def fnFS$*255('&longstring$&';___,ReturnValue,ReturnValue$*255,Index)')
35984     let fnprintline('      gosub FunctionSwitch')
35985     let fnprintline('      let fnFS$ = ReturnValue$')
35986     let fnprintline('   fnend')
35987 ! 
35988     let fnprintline('! ')
35989     let fnprintline('! ')
35990 ! 
35991     let fnsetlinenumber(92000,1)
35992     let longstring$='library "'&setting_fileiopath$&'" : fnOpenFile,Fnclosefile,Fngetfilenumber,Fnkey$,FnBuildKey$,Fnreadlayoutarrays,Fndoeslayoutexist,Fnreadallkeys,fnReadRelativeDescription$,fnReadRelUnopenedDescription$,fnReadRelUnopenedNumber,fnUpdateFile,fnLog,fnLogArray,fnSetLogChanges,fnLogChanges,fnErrLog,'
35993     let longstring$=longstring$&'fnReadLayouts,Fnmakeuniquekey$,FnDisplayLength,FnLength,FnReadDescription$,FnReadUnopenedDescription$,fnReadRecordWhere$,fnUniqueKey,fnReadNumber,fnReadUnopenedNumber,fnReadRelativeNumber,fnNotInFile,fnDataCrawler,fnDataEdit,fnShowData,fnCopyfile'
35994     let fnprintline('FunctionSwitch: ! Routine to call custom function')
35995     let fnprintline('! ')
35996     let fnprintline('   if ~DataIsInside then')
35997     let fnprintline('      let fnPopData(2)')
35998     let fnprintline('      if Function$="{{SetData}}" then let DataIsInside=1')
35999     let fnprintline('   end if')
36000     let fnprintline('! ')
36001     let fnprintline('   if ~FileIOLinkageSet then')
36002     let fnprintline('      '&longstring$)
36003     let fnprintline('      library "'&setting_fileiopath$&'" : fnMakeSubProc,fnReadMatchingKeys,fnReadAllNewKeys,fnReadFilterKeys,fnReadEntireLayout,fnReadLayoutHeader,fnReadSubs,fnReadLayoutPath$,fnReadKeyFiles,fnAskCombo$,fnRunProcFile,fnBuildProcFile,fnReadLockedUsers,fnShowMessage,fnExportListviewCSV')
36004     let fnprintline('      library "'&setting_screeniopath$&'" : fnCallScreen$,fnFindSubscript,fnFm$,fnfm,fnDisplayScreen,fnGetUniqueName$,fnIsInputSpec,fnIsOutputSpec,fnDays,fnBR42,fnAnimate,fnPrepareAnimation,fnCloseAnimation,fnFunctionBase,fnListSpec$')
36005     let fnprintline('      let FileIOLinkageSet=1')
36006     let fnprintline('      for Index=1 to udim(mat Subscripts$) : execute (Subscripts$(Index)) : next Index')
36007     let fnprintline('   end if')
36008     let fnprintline('! ')
36009 ! 
36010     let elseif$="if "
36011     for index=1 to udim(mat function$)
36012       let function$=trim$(function$(index))
36013       if function$(1:1)="[" and pos(function$,"]") then 
36014         let function$=trim$(function$(pos(function$,"]")+1:len(function$)))
36015       end if 
36016       if function$(1:1)="#" then ! We Have A Library Function
36017         let fnprintline('   '&elseif$&'Function$ = "'&srep$(function$,"""","""""")&'" then')
36018         let library$=trim$(function$(2:pos(function$,":")-1))
36019         let funccall$=trim$(function$(pos(function$,":")+1:len(function$)))
36020         if (len(library$)) and ~pos(library$," ") and ~pos(library$,"(") then ! We Have A Properly Formatted Library Function
36021           if pos(funccall$(pos(uprc$(funccall$),"FN"):len(funccall$)),"(") then 
36022             let fnprintline('      library "'&trim$(library$)&'" : '&funccall$(pos(uprc$(funccall$),"FN"):pos(funccall$,"(")-1))
36023           else 
36024             let fnprintline('      library "'&trim$(library$)&'" : '&funccall$(pos(uprc$(funccall$),"FN"):len(funccall$)))
36025           end if 
36026         end if 
36027         if fnstringfunction(funccall$) then 
36028           let fnprintline('      let ReturnValue$ = '&funccall$)
36029         else 
36030           let fnprintline('      let ReturnValue = '&funccall$)
36031         end if 
36032         let elseif$="else if "
36033       end if 
36034       if function$(1:1)="{" then ! We Have A Custom Function
36035         let funccall$=fnreadcustomfunctionstatement$(function$)
36036         if len(trim$(funccall$)) then 
36037           let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36038           if fnstringfunction(funccall$) then 
36039             let fnprintline('      let ReturnValue$ = '&funccall$)
36040           else 
36041             let fnprintline('      let ReturnValue = '&funccall$)
36042           end if 
36043           let elseif$="else if "
36044         end if 
36045       end if 
36046     next index
36047     for index=si_enterfn to si_exitfn
36048       let function$=trim$(screenio$(index))
36049       if function$(1:1)="[" and pos(function$,"]") then 
36050         let function$=trim$(function$(pos(function$,"]")+1:len(function$)))
36051       end if 
36052       if function$(1:1)="#" then ! We Have A Library Function
36053         let fnprintline('   '&elseif$&'Function$ = "'&srep$(function$,"""","""""")&'" then')
36054         let library$=trim$(function$(2:pos(function$,":")-1))
36055         let funccall$=trim$(function$(pos(function$,":")+1:len(function$)))
36056         if (len(library$)) and ~pos(library$," ") and ~pos(library$,"(") then ! We Have A Properly Formatted Library Function
36057           if pos(funccall$(pos(uprc$(funccall$),"FN"):len(funccall$)),"(") then 
36058             let fnprintline('      library "'&trim$(library$)&'" : '&funccall$(pos(uprc$(funccall$),"FN"):pos(funccall$,"(")-1))
36059           else 
36060             let fnprintline('      library "'&trim$(library$)&'" : '&funccall$(pos(uprc$(funccall$),"FN"):len(funccall$)))
36061           end if 
36062         end if 
36063         if fnstringfunction(funccall$) then 
36064           let fnprintline('      let ReturnValue$ = '&funccall$)
36065         else 
36066           let fnprintline('      let ReturnValue = '&funccall$)
36067         end if 
36068         let elseif$="else if "
36069       end if 
36070       if function$(1:1)="{" then ! We Have A Custom Function
36071         let funccall$=fnreadcustomfunctionstatement$(function$)
36072         if len(trim$(funccall$)) then 
36073           let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36074           if fnstringfunction(funccall$) then 
36075             let fnprintline('      let ReturnValue$ = '&funccall$)
36076           else 
36077             let fnprintline('      let ReturnValue = '&funccall$)
36078           end if 
36079           let elseif$="else if "
36080         end if 
36081       end if 
36082     next index
36083 ! 
36084     if exists(setting_functionfolder$) then 
36085       if exists(setting_functionfolder$&"defaults\") then 
36086         if exists(setting_functionfolder$&"defaults\enter.brs") then 
36087           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\enter.brs")
36088           let funccall$=fnreadcustomfunctionstatement$(function$)
36089           if len(trim$(funccall$)) then 
36090             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36091             if fnstringfunction(funccall$) then 
36092               let fnprintline('      let ReturnValue$ = '&funccall$)
36093             else 
36094               let fnprintline('      let ReturnValue = '&funccall$)
36095             end if 
36096             let elseif$="else if "
36097           end if 
36098         end if 
36099         if exists(setting_functionfolder$&"defaults\init.brs") then 
36100           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\init.brs")
36101           let funccall$=fnreadcustomfunctionstatement$(function$)
36102           if len(trim$(funccall$)) then 
36103             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36104             if fnstringfunction(funccall$) then 
36105               let fnprintline('      let ReturnValue$ = '&funccall$)
36106             else 
36107               let fnprintline('      let ReturnValue = '&funccall$)
36108             end if 
36109             let elseif$="else if "
36110           end if 
36111         end if 
36112         if exists(setting_functionfolder$&"defaults\read.brs") then 
36113           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\read.brs")
36114           let funccall$=fnreadcustomfunctionstatement$(function$)
36115           if len(trim$(funccall$)) then 
36116             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36117             if fnstringfunction(funccall$) then 
36118               let fnprintline('      let ReturnValue$ = '&funccall$)
36119             else 
36120               let fnprintline('      let ReturnValue = '&funccall$)
36121             end if 
36122             let elseif$="else if "
36123           end if 
36124         end if 
36125         if exists(setting_functionfolder$&"defaults\load.brs") then 
36126           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\load.brs")
36127           let funccall$=fnreadcustomfunctionstatement$(function$)
36128           if len(trim$(funccall$)) then 
36129             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36130             if fnstringfunction(funccall$) then 
36131               let fnprintline('      let ReturnValue$ = '&funccall$)
36132             else 
36133               let fnprintline('      let ReturnValue = '&funccall$)
36134             end if 
36135             let elseif$="else if "
36136           end if 
36137         end if 
36138         if exists(setting_functionfolder$&"defaults\write.brs") then 
36139           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\write.brs")
36140           let funccall$=fnreadcustomfunctionstatement$(function$)
36141           if len(trim$(funccall$)) then 
36142             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36143             if fnstringfunction(funccall$) then 
36144               let fnprintline('      let ReturnValue$ = '&funccall$)
36145             else 
36146               let fnprintline('      let ReturnValue = '&funccall$)
36147             end if 
36148             let elseif$="else if "
36149           end if 
36150         end if 
36151         if exists(setting_functionfolder$&"defaults\wait.brs") then 
36152           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\wait.brs")
36153           let funccall$=fnreadcustomfunctionstatement$(function$)
36154           if len(trim$(funccall$)) then 
36155             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36156             if fnstringfunction(funccall$) then 
36157               let fnprintline('      let ReturnValue$ = '&funccall$)
36158             else 
36159               let fnprintline('      let ReturnValue = '&funccall$)
36160             end if 
36161             let elseif$="else if "
36162           end if 
36163         end if 
36164         if exists(setting_functionfolder$&"defaults\locked.brs") then 
36165           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\locked.brs")
36166           let funccall$=fnreadcustomfunctionstatement$(function$)
36167           if len(trim$(funccall$)) then 
36168             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36169             if fnstringfunction(funccall$) then 
36170               let fnprintline('      let ReturnValue$ = '&funccall$)
36171             else 
36172               let fnprintline('      let ReturnValue = '&funccall$)
36173             end if 
36174             let elseif$="else if "
36175           end if 
36176         end if 
36177         if exists(setting_functionfolder$&"defaults\merge.brs") then 
36178           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\merge.brs")
36179           let funccall$=fnreadcustomfunctionstatement$(function$)
36180           if len(trim$(funccall$)) then 
36181             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36182             if fnstringfunction(funccall$) then 
36183               let fnprintline('      let ReturnValue$ = '&funccall$)
36184             else 
36185               let fnprintline('      let ReturnValue = '&funccall$)
36186             end if 
36187             let elseif$="else if "
36188           end if 
36189         end if 
36190         if exists(setting_functionfolder$&"defaults\mainloop.brs") then 
36191           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\mainloop.brs")
36192           let funccall$=fnreadcustomfunctionstatement$(function$)
36193           if len(trim$(funccall$)) then 
36194             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36195             if fnstringfunction(funccall$) then 
36196               let fnprintline('      let ReturnValue$ = '&funccall$)
36197             else 
36198               let fnprintline('      let ReturnValue = '&funccall$)
36199             end if 
36200             let elseif$="else if "
36201           end if 
36202         end if 
36203         if exists(setting_functionfolder$&"defaults\nokey.brs") then 
36204           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\nokey.brs")
36205           let funccall$=fnreadcustomfunctionstatement$(function$)
36206           if len(trim$(funccall$)) then 
36207             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36208             if fnstringfunction(funccall$) then 
36209               let fnprintline('      let ReturnValue$ = '&funccall$)
36210             else 
36211               let fnprintline('      let ReturnValue = '&funccall$)
36212             end if 
36213             let elseif$="else if "
36214           end if 
36215         end if 
36216         if exists(setting_functionfolder$&"defaults\prelist.brs") then 
36217           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\prelist.brs")
36218           let funccall$=fnreadcustomfunctionstatement$(function$)
36219           if len(trim$(funccall$)) then 
36220             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36221             if fnstringfunction(funccall$) then 
36222               let fnprintline('      let ReturnValue$ = '&funccall$)
36223             else 
36224               let fnprintline('      let ReturnValue = '&funccall$)
36225             end if 
36226             let elseif$="else if "
36227           end if 
36228         end if 
36229         if exists(setting_functionfolder$&"defaults\postlist.brs") then 
36230           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\postlist.brs")
36231           let funccall$=fnreadcustomfunctionstatement$(function$)
36232           if len(trim$(funccall$)) then 
36233             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36234             if fnstringfunction(funccall$) then 
36235               let fnprintline('      let ReturnValue$ = '&funccall$)
36236             else 
36237               let fnprintline('      let ReturnValue = '&funccall$)
36238             end if 
36239             let elseif$="else if "
36240           end if 
36241         end if 
36242         if exists(setting_functionfolder$&"defaults\exit.brs") then 
36243           let function$=fnfunctionstring$(setting_functionfolder$&"defaults\exit.brs")
36244           let funccall$=fnreadcustomfunctionstatement$(function$)
36245           if len(trim$(funccall$)) then 
36246             let fnprintline('   '&elseif$&'Function$ = "'&function$&'" then')
36247             if fnstringfunction(funccall$) then 
36248               let fnprintline('      let ReturnValue$ = '&funccall$)
36249             else 
36250               let fnprintline('      let ReturnValue = '&funccall$)
36251             end if 
36252             let elseif$="else if "
36253           end if 
36254         end if 
36255       end if 
36256     end if 
36257 ! 
36258     if elseif$="else if " then 
36259       let fnprintline('   else')
36260       let fnprintline('      if Function$<>"{{GetData}}" and Function$<>"{{SetData}}" then')
36261       let fnprintline('         print "Function ("&function$&") Not Supported: The library is out of date or fn not found."')
36262       let fnprintline('      end if')
36263       let fnprintline('   end if')
36264     else 
36265       let fnprintline('   if Function$<>"{{GetData}}" and Function$<>"{{SetData}}" then')
36266       let fnprintline('      print "Function ("&function$&") Not Supported: The library is out of date or fn not found."')
36267       let fnprintline('   end if')
36268     end if 
36269 ! 
36270     let fnprintline('! ')
36271     let fnprintline('   if ~DataIsInside or Function$="{{GetData}}" then')
36272     let fnprintline('      let fnPushData(2)')
36273     let fnprintline('      let DataIsInside=0')
36274     let fnprintline('   end if')
36275 ! 
36276     let fnprintline('return')
36277     let fnprintline('! ')
36278 ! 
36279 ! .   ! Print End of Program
36280     let fnsetlinenumber(99000,10)
36281     let fnprintline('OPEN: !   ***** Function  To Call Library Openfile And Proc Subs')
36282     let fnprintline('   def Fnopen(Filename$*255, Mat F$, Mat F, Mat Form$; Inputonly, Keynum, Dont_Sort_Subs, Path$*255, Mat Descr$, Mat Field_Widths,Supress_Prompt,Ignore_Errors,___,Index)')
36283     let fnprintline('      dim _FileIOSubs$(1)*800, _loadedsubs$(1)*32')
36284     let fnprintline('      let Fnopen=Fnopenfile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$)')
36285     let fnprintline('      if srch(_loadedsubs$,uprc$(Filename$))<=0 then : mat _loadedsubs$(UDIM(_loadedsubs$)+1) : _loadedsubs$(UDIM(_loadedsubs$))=uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index')
36286     let fnprintline('   fnend')
36287     let fnprintline('! ')
36288 ! 
36289     let fnsetlinenumber(99980,10)
36290     let fnprintline('IGNORE: continue')
36291     let fnprintline('REPEAT: retry')
36292 ! 
36293     close #filenumber: 
36294 ! 
36295     if exists(setting_screenfolder$&"\popmem") then 
36296       execute "copy "&setting_screenfolder$&"\popmem "&setting_screenfolder$&"\popmem.$$$"
36297     else 
36298       execute "list pack endpack >"&setting_screenfolder$&"\popmem.$$$"
36299       execute "list ScreenIODims endScreenIODims >>"&setting_screenfolder$&"\popmem.$$$"
36300       execute "list ScreenDesignerDims endScreenDesignerDims >>"&setting_screenfolder$&"\popmem.$$$"
36301     end if 
36302 ! 
36303 ! .   ! Build Compile Proc
36304     open #(filenumber:=fngetfilenumber): "name="&filename$&", replace", display, output 
36305     print #filenumber: "proc noecho"
36306     print #filenumber: "subproc "&setting_screenfolder$&"\"&lwrc$(trim$(screenio$(si_screencode)))&".brs"
36307     print #filenumber: "subproc "&setting_screenfolder$&"\popmem.$$$"
36308     if exists(setting_screenfolder$&"\"&lwrc$(trim$(screenio$(si_screencode)))&".br") then 
36309       print #filenumber: "replace "&setting_screenfolder$&"\"&lwrc$(trim$(screenio$(si_screencode)))&".br"
36310     else 
36311       print #filenumber: "save "&setting_screenfolder$&"\"&lwrc$(trim$(screenio$(si_screencode)))&".br"
36312     end if 
36313     print #filenumber: "skip 4 if err==0"
36314     print #filenumber: "skip 1 if err<>302"
36315     print #filenumber: "pause"
36316     print #filenumber: "print 'Error #'&str$(err)&' in Screen: "&trim$(screenio$(si_screencode))&". Press enter to continue.'"
36317     print #filenumber: "pause"
36318     print #filenumber: "system"
36319     close #filenumber: 
36320 ! 
36321 ! .   ! Compile Helper Library
36322     execute "system -C -M "&fngetbrexe$&" -"&fngetwbcfg$&" proc "&filename$
36323     if waitforcomplete then 
36324       let fnwaitforfile(dummy$,"",filename$)
36325     end if 
36326   fnend 
36327 ! 
36328 PRINTLINE: ! Prints A Line With A Line Number
36329   def fnprintline(string$*800;skipnumber)
36330     if skipnumber then 
36331       print #static_compilefile: "      "&string$
36332     else 
36333       print #static_compilefile: cnvrt$("PIC(#####)",static_linenumber)&" "&string$
36334       let static_linenumber+=static_increment
36335     end if 
36336   fnend 
36337 SETLINENUMBER: ! Sets The Printline Linenumber, Increment, Filenumber
36338   def fnsetlinenumber(linenumber,increment;filenumber)
36339     let static_increment=increment
36340     let static_linenumber=linenumber
36341     if filenumber then 
36342       let static_compilefile=filenumber
36343     end if 
36344   fnend 
36345 ! 
36346 ! 
36347   dim const$(1)*1024, constname$(1)*255
36348   dim currentselect$*255, currentcase$(1)*255
36349   dim define$, include$, select$, case$, caseelse$, endselect$
36350 ! 
36351 IMPORTFUNCTION: ! Imports The Function Text From A .Brs File
36352   def fnimportfunction(function$*255,mat newinclude$;___,filenumber,string$*800, constindex, constantposition, skipnextone, constnamestartpos, constnameendpos, selectposition,caseposition,caseindex,currentcasechunk,nextcasechunk)
36353     let function$=fncustomfilenameof$(function$)
36354 ! 
36355     open #(filenumber:=fngetfilenumber): "name="&function$,display,input error ignore
36356     if file(filenumber)=0 then 
36357 ! 
36358 ! .      ! Define constants apply to individual functions only, so clear them out each time
36359       mat const$(0)
36360       mat constname$(0)
36361 ! 
36362 ! .      ! Set Search Terms so they don't screw with LEXI
36363       let include$="#INC"&"LUDE"
36364       let define$="#DEF"&"INE#"
36365       let select$="#SEL"&"ECT#"
36366       let case$="#CA"&"SE#"
36367       let caseelse$="#CASE"&" ELSE#"
36368       let endselect$="#END"&" SELECT#"
36369 ! 
36370       let fnprintline('! Imported From "'&function$&'"')
36371       do while file(filenumber)=0
36372         linput #filenumber: string$ eof ignore
36373         if file(filenumber)=0 then 
36374 ! .            ! Maintain Spacing
36375           if trim$(string$)="" then let string$=" !"
36376 ! 
36377 ! .            ! Process Include Statements
36378           if pos(uprc$(string$),include$) then 
36379             mat newinclude$(udim(mat newinclude$)+1)
36380             let newinclude$(udim(mat newinclude$))=trim$(string$(pos(lwrc$(string$),"#include")+8:len(string$)))
36381           end if 
36382 ! 
36383 ! .            ! Handle LEXI stuff
36384 ! .            ! Check for Constants
36385           for constindex=1 to udim(mat const$)
36386             if (constantposition:=pos(uprc$(string$),uprc$(constname$(constindex)))) then 
36387               let string$=string$(1:constantposition-1) & const$(constindex) & string$(constantposition+len(constname$(constindex)):len(string$))
36388             end if 
36389           next constindex
36390 ! 
36391 ! .            ! Check for DEFINE constant declarations
36392           if (constantposition:=pos(uprc$(string$),define$)) then 
36393             let constantposition+=8
36394             if (constnamestartpos:=pos(string$,"[[",constantposition)) then 
36395               if (constnameendpos:=pos(string$,"]]",constnamestartpos)) then 
36396                 let constnameendpos+=1
36397                 mat const$(constindex:=(udim(mat const$)+1))
36398                 mat constname$(constindex)
36399                 let constname$(constindex)=string$(constnamestartpos:constnameendpos)
36400                 let const$(constindex)=trim$(string$(constnameendpos+2:len(string$)))
36401                 if const$(constindex)(1:1)="=" then ! If Equals, Then Ignore It
36402                   let const$(constindex)=trim$(const$(constindex)(2:len(const$(constindex))))
36403                 end if 
36404                 if const$(constindex)(1:1)='"' and const$(constindex)(len(const$(constindex)):len(const$(constindex)))='"' then 
36405                   let const$(constindex)=const$(constindex)(2:len(const$(constindex))-1) ! Remove Quotes If Both Are Present
36406                 end if 
36407               end if 
36408             end if 
36409           end if 
36410 ! 
36411 ! .            ! Check for SELECT CASE statements
36412           if (selectposition:=pos(uprc$(string$),select$)) then 
36413             if (caseposition:=pos(uprc$(string$),case$,selectposition)) then 
36414               let currentselect$=string$(selectposition+8:caseposition-1)
36415               let caseindex=0
36416               let currentcasechunk=caseposition+6
36417               do 
36418                 let caseindex+=1
36419                 mat currentcase$(caseindex)
36420                 if (nextcasechunk:=pos(string$,"#",currentcasechunk)) then 
36421                   let currentcase$(caseindex)=string$(currentcasechunk:nextcasechunk-1)
36422                   let currentcasechunk=nextcasechunk+1
36423                 else 
36424                   let currentcase$(caseindex)=string$(currentcasechunk:len(string$))
36425                 end if 
36426               loop while nextcasechunk
36427               let string$=string$(1:selectposition-1) & "if "
36428               for caseindex=1 to udim(mat currentcase$)
36429                 if caseindex>1 then 
36430                   let string$=string$ & " or "
36431                 end if 
36432                 let string$=string$ & trim$(currentselect$) & " = " & trim$(currentcase$(caseindex))
36433               next caseindex
36434               let string$ = string$ & " then"
36435             end if 
36436           else if (caseposition:=pos(uprc$(string$),case$)) then 
36437             if len(trim$(currentselect$)) then 
36438               let caseindex=0
36439               let currentcasechunk=caseposition+6
36440               do 
36441                 let caseindex+=1
36442                 mat currentcase$(caseindex)
36443                 if (nextcasechunk:=pos(string$,"#",currentcasechunk)) then 
36444                   let currentcase$(caseindex)=string$(currentcasechunk:nextcasechunk-1)
36445                   let currentcasechunk=nextcasechunk+1
36446                 else 
36447                   let currentcase$(caseindex)=string$(currentcasechunk:len(string$))
36448                 end if 
36449               loop while nextcasechunk
36450               let string$=string$(1:caseposition-1) & "else if "
36451               for caseindex=1 to udim(mat currentcase$)
36452                 if caseindex>1 then 
36453                   let string$=string$ & " or "
36454                 end if 
36455                 let string$=string$ & trim$(currentselect$) & " = " & trim$(currentcase$(caseindex))
36456               next caseindex
36457               let string$ = string$ & " then"
36458             end if 
36459           else if (caseposition:=pos(uprc$(string$),caseelse$)) then 
36460             if len(trim$(currentselect$)) then 
36461               let string$ = string$(1:caseposition-1) & "else"
36462             end if 
36463           else if (endposition:=pos(uprc$(string$),endselect$)) then 
36464             if len(trim$(currentselect$)) then 
36465               let string$ = string$(1:endposition-1) & "end if"
36466               let currentselect$ = ""
36467             end if 
36468           end if 
36469 ! 
36470 ! .            ! Print Line to File
36471           let fnprintline(string$,skipnextone)
36472 ! 
36473 ! .            ! Check if we need to Skip Next One
36474           if trim$(string$)(len(trim$(string$))-1:len(trim$(string$))) = "!:" then 
36475             let skipnextone=1
36476           else 
36477             let skipnextone=0
36478           end if 
36479 ! 
36480         end if 
36481       loop 
36482       close #filenumber: 
36483     end if 
36484   fnend 
36485 ! 
36486   def fnfindincludes(function$*255,mat newinclude$;___,filenumber,string$*800,fi_function$*255)
36487     let function$=fncustomfilenameof$(function$)
36488 ! 
36489     open #(filenumber:=fngetfilenumber): "name="&function$,display,input error ignore
36490     if file(filenumber)=0 then 
36491 ! 
36492 ! .      ! Set Search Terms so they don't screw with LEXI
36493       let include$="#INC"&"LUDE"
36494 ! 
36495       do while file(filenumber)=0
36496         linput #filenumber: string$ eof ignore
36497         if file(filenumber)=0 then 
36498 ! 
36499 ! .            ! Process Include Statements
36500           if pos(uprc$(string$),include$) then 
36501             let fi_function$=lwrc$(trim$(string$(pos(lwrc$(string$),"#include")+8:len(string$))))
36502             if srch(mat newinclude$,fi_function$)<=0 then 
36503               mat newinclude$(udim(mat newinclude$)+1)
36504               let newinclude$(udim(mat newinclude$))=fi_function$
36505             end if 
36506           end if 
36507         end if 
36508       loop 
36509       close #filenumber: 
36510     end if 
36511   fnend 
36512 ! 
36513 CUSTOMFILENAMEOF: ! Extracts The File Name For A {Filename} Style Fn Reference
36514   def fncustomfilenameof$*255(function$*255)
36515     let function$=lwrc$(function$)
36516     if ~exists(setting_functionfolder$) then execute "mkdir function"
36517     let function$=trim$(function$)
36518     if function$(1:1)="{" then 
36519       let function$=function$(2:len(function$))
36520     end if 
36521     if function$(len(function$):len(function$))="}" then 
36522       let function$=function$(1:len(function$)-1)
36523     end if 
36524     let function$=setting_functionfolder$&function$&".brs"
36525     let fncustomfilenameof$=function$
36526   fnend 
36527   def fnfunctionstring$*255(filename$*255)
36528     let filename$=srep$(lwrc$(filename$),lwrc$(setting_functionfolder$),"")
36529     let filename$=srep$(lwrc$(filename$),".brs","")
36530     let filename$="{"&filename$&"}"
36531     let fnfunctionstring$=filename$
36532   fnend 
36533 ! 
36534 READAVAILABLEFUNCTIONS: ! Reads All Helper Function Files Into An Array
36535   def fnreadavailablefunctions(mat functions$;___,dirfile,dummy$*255)
36536     let fnreadfiles(mat functions$,setting_functionfolder$,"brs",1,1)
36537   fnend 
36538 ! 
36539   def fnreadfiles(mat files$,folder$*255;extension$,cleararray,createfolder,includeextensions,&mask$,___,dirfile,dummy$*255,dirmask$*400)
36540     if cleararray then mat files$(0)
36541 ! 
36542     if createfolder then 
36543       if ~exists(folder$) then execute "mkdir "&folder$
36544     end if 
36545 ! 
36546     if folder$(len(folder$):len(folder$))="\" then let folder$=folder$(1:len(folder$)-1)
36547 ! 
36548     if ~len(trim$(extension$)) then let extension$="*"
36549 ! 
36550     if len(trim$(mask$)) then 
36551       let dirmask$=folder$&"\"&fncleanmask$(mask$)&"."&lwrc$(extension$)
36552     else 
36553       let dirmask$=folder$&"\*."&lwrc$(extension$)
36554     end if 
36555 ! 
36556     execute "dir "&dirmask$&" >dirlist.[SESSION]" error ignore
36557     open #(dirfile:=fngetfilenumber) : "Name=dirlist.[SESSION]", display,input 
36558     do 
36559       linput #dirfile: dummy$ eof ignore
36560       if file(dirfile)=0 and ~pos(dummy$,"<DIR>") then 
36561         if extension$="*" or uprc$(dummy$(11:13))=uprc$(extension$) then 
36562           mat files$(udim(files$)+1) !:
                let files$(udim(files$))=lwrc$(trim$(dummy$(44:99))(1:32))
36563           if pos(lwrc$(files$(udim(files$))),".")>0 then 
36564             let files$(udim(files$))=files$(udim(files$))(1:pos(lwrc$(files$(udim(files$))),".",-1)-1)
36565             if includeextensions then 
36566               if extension$="*" then 
36567                 let files$(udim(files$))=files$(udim(files$))&"."&lwrc$(dummy$(11:13))
36568               else 
36569                 let files$(udim(files$))=files$(udim(files$))&"."&extension$
36570               end if 
36571             end if 
36572           end if 
36573         end if 
36574       end if 
36575     loop while file(dirfile)=0
36576     close #dirfile: 
36577     execute "free dirlist.[SESSION]"
36578   fnend 
36579 ! 
36580   def fncleanmask$*80(&mask$;___,index,outmask$*80)
36581     let mask$=trim$(mask$)
36582     if pos(mask$,"\") then 
36583       let mask$=mask$(pos(mask$,"\"):len(mask$))
36584     end if 
36585     if pos(mask$,".") then 
36586       let mask$=mask$(1:pos(mask$,"."))
36587     end if 
36588     let outmask$=""
36589     for index=1 to len(mask$)
36590       if pos("abcdefghijklmnopqrstuvwxyz?*",lwrc$(mask$(index:index))) then 
36591         let outmask$=outmask$&mask$(index:index)
36592       end if 
36593     next index
36594     let mask$=outmask$
36595     let fncleanmask$=outmask$
36596   fnend 
36597 ! 
36598 READCUSTOMFUNCTION: ! Reads The Given Custom Function Into A Long String With Embedded Crs (Chr$(13))
36599   def fnreadcustomfunction$*2000(filename$*255;___,filenumber,string$*800,functiontext$*2000,done)
36600     let filename$=fncustomfilenameof$(filename$)
36601 ! 
36602     if exists(filename$) then 
36603       open #(filenumber:=fngetfilenumber): "name="&filename$,display,input error ignore
36604       if file(filenumber)=0 then 
36605         do while file(filenumber)=0
36606           linput #filenumber: string$ eof ignore
36607           if file(filenumber)=0 then 
36608             if len(string$)+1+len(functiontext$)<2000 then 
36609               let functiontext$=functiontext$&hex$("0D0A")&string$
36610             else 
36611               let done=1
36612             end if 
36613           end if 
36614         loop until done
36615         close #filenumber: 
36616       end if 
36617     end if 
36618     let fnreadcustomfunction$=functiontext$
36619   fnend 
36620 ! 
36621 READCUSTOMFUNCTIONSTATEMENT: ! Reads And Calculates The Custom Function Call Statement From The Given File
36622   def fnreadcustomfunctionstatement$*255(filename$*255;___,filenumber,string$*800,functioncall$*800,done,starfound,comma,semicolon,leftparen,rightparen,numberend)
36623 ! 
36624     let filename$=fncustomfilenameof$(filename$)
36625 ! 
36626     if exists(filename$) then 
36627       open #(filenumber:=fngetfilenumber): "name="&filename$,display,input error ignore
36628       if file(filenumber)=0 then 
36629         do while file(filenumber)=0
36630           linput #filenumber: string$ eof ignore
36631           if file(filenumber)=0 then 
36632             if pos(lwrc$(string$),"def fn") then 
36633               let functioncall$=string$
36634               let done=1
36635             end if 
36636           end if 
36637         loop until done
36638         close #filenumber: 
36639       end if 
36640     end if 
36641 ! 
36642     if len(trim$(functioncall$)) then 
36643       let functioncall$=functioncall$(pos(lwrc$(functioncall$),"def fn")+4:len(functioncall$))
36644       if pos(functioncall$,"(;") then let functioncall$=functioncall$(1:pos(functioncall$,"(;")-1)
36645       if pos(functioncall$,";") then let functioncall$=functioncall$(1:pos(functioncall$,";")-1)&")"
36646       let functioncall$=srep$(functioncall$,"&","")
36647 ! 
36648       do while (starfound:=pos(functioncall$,"*"))
36649         let comma=pos(functioncall$,",",starfound)
36650         let semicolon=pos(functioncall$,";",starfound)
36651         let leftparen=pos(functioncall$,"(",starfound)
36652         let rightparen=pos(functioncall$,")",starfound)
36653         if comma<starfound then let comma=9999
36654         if semicolon<starfound then let semicolon=9999
36655         if leftparen<starfound then let leftparen=9999
36656         if rightparen<starfound then let rightparen=9999
36657         let numberend=min(comma,semicolon,leftparen,rightparen)-1
36658         let functioncall$(starfound:numberend)=""
36659       loop 
36660     end if 
36661 ! 
36662     let fnreadcustomfunctionstatement$=functioncall$
36663 ! 
36664   fnend 
36665 ! 
36666 CREATENEWFUNCTION: ! Creates A New Blank Function File
36667   def fncreatenewfunction(function$*255;functiontype,subtype,___,filename$*255,filenumber)
36668 ! 
36669     let filename$=fncustomfilenameof$(function$)
36670     let function$=trim$(function$)
36671     if function$(1:1)="{" then let function$=function$(2:len(function$))
36672     if function$(len(function$):len(function$))="}" then let function$=function$(1:len(function$)-1)
36673 ! 
36674     if ~exists(filename$) then 
36675       open #(filenumber:=fngetfilenumber): "name="&filename$&",new,recl=800",display,output error ignore
36676       if file(filenumber)=0 then 
36677         print #filenumber: " ! "&filename$
36678         print #filenumber: " ! Created on "&date$("mm/dd/ccyy")
36679         print #filenumber: " !"
36680         print #filenumber: " ! fn"&function$&" - This function ..."
36681         print #filenumber: " !"
36682 ! 
36683         if setting_detailedcomments then 
36684           let fnprintcustomfunctioncomment(filenumber,functiontype,subtype)
36685         end if 
36686 ! 
36687         print #filenumber: " !"
36688         print #filenumber: " def fn"&function$
36689         print #filenumber: 
36690         print #filenumber: 
36691         print #filenumber: " fnend"
36692 ! 
36693         close #filenumber: 
36694       end if 
36695     end if 
36696   fnend 
36697 ! 
36698   def fnprintcustomfunctioncomment(filenumber,functiontype,subtype)
36699     print #filenumber: " !"
36700     print #filenumber: " ! Fill in the comment above with a description of what this"
36701     print #filenumber: " ! particular function does."
36702     print #filenumber: " !"
36703     print #filenumber: " !"
36704     if functiontype = 0 then ! #Select# FunctionType #Case# 0
36705     else if functiontype = 1000+si_enterfn then ! #Case# 1000+si_enterfn
36706       print #filenumber: " ! This is an Enter Event Function. When selected as the Enter"
36707       print #filenumber: " ! Event for your screen, this function runs after the screen"
36708       print #filenumber: " ! has loaded but before the screen is drawn and before the"
36709       print #filenumber: " ! record is read."
36710     else if functiontype = 1000+si_initfn then ! #Case# 1000+si_initfn
36711       print #filenumber: " ! This is an Initialize Event Function. When selected as the Init"
36712       print #filenumber: " ! Event for your screen, this function runs only when the user is"
36713       print #filenumber: " ! adding a new record to the data file. Use this event in your Add/Edit"
36714       print #filenumber: " ! screens to initialize the new record and preform any other tasks that"
36715       print #filenumber: " ! need to run each time a record is added. It is not run when Reading"
36716       print #filenumber: " ! an existing record (See the Read Event for that)."
36717     else if functiontype = 1000+si_readfn then ! #Case# 1000+si_readfn
36718       print #filenumber: " ! This is a Read Event Function."
36719       print #filenumber: " ! "
36720       print #filenumber: " ! When used in an Add/Edit screen, this function runs"
36721       print #filenumber: " ! only when the user is editing an existing record in"
36722       print #filenumber: " ! a data file. Use this event to unpack the new record"
36723       print #filenumber: " ! and preform any other tasks that need to run at read time."
36724       print #filenumber: " ! This function runs just after the record is read. It is"
36725       print #filenumber: " ! not run when Adding a record. (See Initialize Event)"
36726       print #filenumber: " !"
36727       print #filenumber: " ! When used in a Listview screen, this function runs each time"
36728       print #filenumber: " ! the user's selection on the listview has changed. You can use it"
36729       print #filenumber: " ! to update related fields when the listview selection changes"
36730       print #filenumber: " ! in realtime. It can slow things down, so use it with caution"
36731       print #filenumber: " ! and make sure your code here is as effecient as possible."
36732     else if functiontype = 1000+si_loadfn then ! #Case# 1000+si_loadfn
36733       print #filenumber: " ! This is a Load Event Function. It runs after the record is"
36734       print #filenumber: " ! read and the screen has finished loading, but before the"
36735       print #filenumber: " ! screen is run. Use it to perform user interactions and other"
36736       print #filenumber: " ! tasks that you want to run after the screen is drawn and"
36737       print #filenumber: " ! before starting your program."
36738     else if functiontype = 1000+si_writefn then ! #Case# 1000+si_writefn
36739       print #filenumber: " ! This is a PreWrite Event Function. When used in an Add/Edit screen,"
36740       print #filenumber: " ! this function runs only when the user exited the screen and chose"
36741       print #filenumber: " ! to save the data. This function is run before the data is written."
36742       print #filenumber: " !"
36743       print #filenumber: " ! This is a good place to do full record validations that can't be"
36744       print #filenumber: " ! done as you're going along. You can cancel the Exit by setting"
36745       print #filenumber: " ! ExitMode to 0."
36746     else if functiontype = 1000+si_waitfn then ! #Case# 1000+si_waitfn
36747       print #filenumber: " ! This is a Wait Event Function. It is called when the user has"
36748       print #filenumber: " ! been idle for a long time (configurable in Screen Attributes)."
36749       print #filenumber: " ! If not specified, the default ScreenIO Idle Message window"
36750       print #filenumber: " ! appears instead. Ues this function to change the default"
36751       print #filenumber: " ! idle behavior for your program."
36752     else if functiontype = 1000+si_lockedfn then ! #Case# 1000+si_lockedfn
36753       print #filenumber: " ! This is a Locked Event Function. Use this to override the default"
36754       print #filenumber: " ! ScreenIO Locked Record behavior. This is run whenever a user attempts"
36755       print #filenumber: " ! to edit a record but the record is locked (currently being edited) by"
36756       print #filenumber: " ! another user."
36757     else if functiontype = 1000+si_mergefn then ! #Case# 1000+si_mergefn
36758       print #filenumber: " ! This is a Merge Event Function. Its called when standard record"
36759       print #filenumber: " ! locking is disabled and instead multiple users are allowed to"
36760       print #filenumber: " ! edit the record, and the system attempts to merge their changes."
36761       print #filenumber: " ! If you select that option, screenIO automatically merges the changes."
36762       print #filenumber: " ! You can use this Merge function to override the default ScreenIO"
36763       print #filenumber: " ! merging behavior."
36764     else if functiontype = 1000+si_loopfn then ! #Case# 1000+si_loopfn
36765       print #filenumber: " ! This is a Main Loop Event Function. This code is run every time"
36766       print #filenumber: " ! the user does anything on your screen. Use it to handle special"
36767       print #filenumber: " ! user interactions such as custom Fkeys and double clicks."
36768     else if functiontype = 1000+si_nokeyfn then ! #Case# 1000+si_nokeyfn
36769       print #filenumber: " ! This is a NoKey Event Function. This function is called"
36770       print #filenumber: " ! any time the selected key is not found when attempting to"
36771       print #filenumber: " ! edit. If this function is not specified, ScreenIO Defaults"
36772       print #filenumber: " ! to displaying a message to the end user indicating that the"
36773       print #filenumber: " ! key could not be found."
36774     else if functiontype = 1000+si_prelistviewfn then ! #Case# 1000+si_prelistviewfn
36775       print #filenumber: " ! This is a Pre-Listview Populate Event Function. When used in"
36776       print #filenumber: " ! a Listview Screen, this code is run just prior to the listview"
36777       print #filenumber: " ! being populated. Use this function to open related reference"
36778       print #filenumber: " ! data files that you want to read during the populate, or to"
36779       print #filenumber: " ! preposition the record pointer in the data file."
36780     else if functiontype = 1000+si_postlistviewfn then ! #Case# 1000+si_postlistviewfn
36781       print #filenumber: " ! This is a Post-Populate Listview Event Function. When used in"
36782       print #filenumber: " ! a Listview Screen, this code runs just after the listview is"
36783       print #filenumber: " ! populated. Use it to close data files you might have opened"
36784       print #filenumber: " ! during Pre-Populate."
36785     else if functiontype = 1000+si_exitfn then ! #Case# 1000+si_exitfn
36786       print #filenumber: " ! This is an Exit Event Function. You can use it to close data"
36787       print #filenumber: " ! files that you opened during the Enter event. It runs when the"
36788       print #filenumber: " ! screen is already closing, and the record has already been"
36789       print #filenumber: " ! written. It is too late to cancel the close here."
36790     else if functiontype = 2000+sf_function then ! #Case# 2000+sf_function
36791       if ~subtype or subtype=1 then 
36792         print #filenumber: " ! This is a Click Event function. It runs when the user Clicks"
36793         print #filenumber: " ! on the Control it is the click event for."
36794       else if subtype=2 then 
36795         print #filenumber: " ! This is a Validate Event Function. This function runs any time"
36796         print #filenumber: " ! the control has been changed by the user."
36797         print #filenumber: " !"
36798         print #filenumber: " ! (To make it run as soon as it is changed, consider adding the"
36799         print #filenumber: " ! ""X"" attribute to the control.)"
36800       else if subtype=3 then 
36801         print #filenumber: " ! This function is both the Populate and Validate functions"
36802         print #filenumber: " ! for a Combo Box control. To use it, set the contents of an"
36803         print #filenumber: " ! array called ReturnData$ and ScreenIO will populate that"
36804         print #filenumber: " ! data into the Combo Box."
36805         print #filenumber: " !"
36806         print #filenumber: " ! It is also a Validate event, so make sure to have your function"
36807         print #filenumber: " ! return true in order accept the users changes to the control."
36808       else if subtype=4 then 
36809         print #filenumber: " ! This function is the Filter function for the listview. It is run"
36810         print #filenumber: " ! as the listview is being populated, and it runs once for every"
36811         print #filenumber: " ! record in the data file, so be careful to make your code run as"
36812         print #filenumber: " ! quickly as possible."
36813         print #filenumber: " !"
36814         print #filenumber: " ! Here you can unpack special fields from the data on disk to"
36815         print #filenumber: " ! the listview if you want to, or color the row by returning html"
36816         print #filenumber: " ! or an [ATTRIBUTE] substitution from your brconfig.sys (or"
36817         print #filenumber: " ! settings.sys) file."
36818         print #filenumber: " !"
36819         print #filenumber: " ! You can also test the data and include or exclude rows in the data"
36820         print #filenumber: " ! file. If your Filter function returns 1 (True) or non-blank (for example,"
36821         print #filenumber: " ! a Color or [Attribute]), then the row is included in the listview."
36822         print #filenumber: " ! If your function returns 0 (False) or Blank, then the row is excluded."
36823         print #filenumber: " !"
36824         print #filenumber: " ! Make sure your filter function returns something. If you want to"
36825         print #filenumber: " ! see all the rows in the list, make your function return 1 (True)."
36826       else if subtype=5 then 
36827         print #filenumber: " ! This function should not be used for Child Screen controls. Instead"
36828         print #filenumber: " ! of linking to a custom function, use the field to pass in parameters"
36829         print #filenumber: " ! to your Child Screen. "
36830       end if 
36831     else if functiontype = 2000+sf_cnvrtin then ! #Case# 2000+sf_cnvrtin
36832       print #filenumber: " ! This is a ConvertIn Function. Normally, you would specify a"
36833       print #filenumber: " ! ScreenIO Conversion Spec such as FMT(, PIC( or DATE( instead"
36834       print #filenumber: " ! of linking to a custom function. This custom function won't"
36835       print #filenumber: " ! be run by ScreenIO."
36836     else if functiontype = 2000+sf_cnvrtout then ! #Case# 2000+sf_cnvrtout
36837       print #filenumber: " ! This is a ConvertOut Function. It isn't supported yet."
36838       print #filenumber: " ! This custom function won't be run by ScreenIO."
36839     end if  ! #End Select#
36840   fnend 
36841 ! 
36842   def fnsubtype(fieldtype$)
36843     if lwrc$(trim$(fieldtype$)) = "p" or lwrc$(trim$(fieldtype$)) = "button" or lwrc$(trim$(fieldtype$)) = "caption" or lwrc$(trim$(fieldtype$)) = "calendar" then ! #Select# lwrc$(trim$(FieldType$)) #Case# "p" # "button" # "caption" # "calendar"
36844       let fnsubtype=1
36845     else if lwrc$(trim$(fieldtype$)) = "c" or lwrc$(trim$(fieldtype$)) = "search" or lwrc$(trim$(fieldtype$)) = "filter" or lwrc$(trim$(fieldtype$)) = "check" then ! #Case# "c" # "search" # "filter" # "check"
36846       let fnsubtype=2
36847     else if lwrc$(trim$(fieldtype$)) = "combo" then ! #Case# "combo"
36848       let fnsubtype=3
36849     else if lwrc$(trim$(fieldtype$)) = "listview" then ! #Case# "listview"
36850       let fnsubtype=4
36851     else if lwrc$(trim$(fieldtype$)) = "screen" then ! #Case# "screen"
36852       let fnsubtype=5
36853     else ! #Case Else#
36854       let fnsubtype=0
36855     end if  ! #End Select#
36856   fnend 
36857 ! 
36858 EDITCUSTOMFUNCTION: ! Launches Myedit To Edit The Given Function File
36859   def fneditcustomfunction(filename$*255)
36860 ! 
36861     let filename$=fncustomfilenameof$(filename$)
36862 ! 
36863     if exists(filename$) then 
36864       execute "system -C -M start "&os_filename$(filename$)
36865     end if 
36866   fnend 
36867 ! 
36868   def library fnselectevent$*255(&current$;&returnfkey)
36869     let fnselectevent$=fnselectfunctiondialog$(current$,returnfkey)
36870   fnend 
36871 ! 
36872 SELECTFUNCTIONDIALOG: ! Displays The Select Function Dialog.
36873   def fnselectfunctiondialog$*255(current$*255;&returnfkey,functiontype,subtype,___,screenrows,screencols,rpos,cpos,window,textboxwindow,selectedfunction$*255,functiontext$*2000,currentselection,newselection,currentfield,buttontext$,tempselection,index,done)
36874 ! 
36875     dim lvsf_name$(1)
36876     dim lvsf_width(1)
36877     dim lvsf_spec$(1)
36878     dim lvsf_fnfl$(1)*80
36879 ! 
36880     let lvsf_name$(1)="Function Files"
36881     let lvsf_width(1)=25
36882     let lvsf_spec$(1)="C 80,X"
36883 ! 
36884     let selectedfunction$=fncustomfilenameof$(lwrc$(current$(1:28)))
36885     let selectedfunction$=selectedfunction$(10:len(selectedfunction$)-4)
36886 ! 
36887     let fnreadavailablefunctions(mat lvsf_fnfl$)
36888 ! 
36889     let newselection=1
36890     let currentfield=1
36891 ! 
36892     let fngetscreensize(screenrows,screencols)
36893     if ~screenrows or ~screencols then 
36894       let fnreadscreensize(screenrows,screencols)
36895     end if 
36896 ! 
36897     let rpos=int((screenrows-24)/2)
36898     let cpos=int((screencols-85)/2)
36899 ! 
36900     open #(window:=fngetfilenumber) : "srow="&str$(rpos)&",scol="&str$(cpos)&",rows=24,cols=85,border=S,caption=Select Function", display, outin 
36901     open #(textboxwindow:=fngetfilenumber) : "srow=2,scol=28,rows=20,cols=56,parent="&str$(window),display,outin 
36902 ! 
36903     print #window, fields "2,2,LIST 20/25,HEADERS,/W:W" : (mat lvsf_name$,mat lvsf_width,mat lvsf_spec$)
36904     print #window, fields "2,2,LIST 20/25,=" : (mat lvsf_fnfl$)
36905     print #window, fields "2,2,LIST 20/25,SORT" : 1
36906     print #window, fields "23,42,CC 6,/W:W,B34;23,50,CC 6,/W:W,B35;23,58,CC 6,/W:W,B99;23,66,12/CC 16,/W:W,B37" : "New","Ok","Cancel","Affected Screens"
36907     print #window, fields "23,2,P 1/2,,1004" : setting_imagepath$&"\search.png"
36908 ! 
36909     execute "config keyboard clear"
36910     execute "config keyboard 0B00 0A0A0A636F6E206B657920636C6561720D" ! Shift F1 Key To "con key clear<CR>"
36911 ! 
36912     execute "config keyboard 0B 0F00" ! Up To Shift F5 (Fkey 15)
36913     execute "config keyboard 0A 1000" ! Dn To Shift F6 (Fkey 16)
36914 ! 
36915     execute "config keyboard 09 1100" ! Tab To Shift F7 (Fkey 17)
36916     execute "config keyboard 07 1100" ! Shift Tab To Shift F7 (Fkey 17)
36917 ! 
36918     do 
36919       if udim(mat lvsf_fnfl$) then 
36920         if newselection<>currentselection then 
36921           let functiontext$=fnreadcustomfunction$(lvsf_fnfl$(newselection))
36922           if fn42ia then 
36923             let functiontext$=srep$(functiontext$,hex$("0D0A"),hex$("0D"))
36924             let functiontext$=srep$(functiontext$,hex$("0A"),hex$("0D"))
36925             let functiontext$=srep$(functiontext$,hex$("0D"),hex$("0D0A"))
36926             if fn42jd then 
36927               print #textboxwindow, fields "1,1,1120/C 2000,S^ENTER_CRLF" : functiontext$
36928             else 
36929               print #textboxwindow, fields "1,1,1120/C 2000,S^ENTER-CRLF" : functiontext$
36930             end if 
36931           else 
36932             print #textboxwindow, fields "1,1,1120/C 2000,S" : functiontext$
36933           end if 
36934           let currentselection=newselection
36935         end if 
36936       end if 
36937 ! 
36938       if currentfield=2 then 
36939         let curfld(currentfield,currentselection)
36940       else 
36941         let curfld(currentfield)
36942       end if 
36943 ! 
36944       if udim(mat lvsf_fnfl$) then 
36945         print #window, fields "23,5,35/SEARCH 28,X,2,2" : selectedfunction$
36946         if currentfield=1 then 
36947           input #window, fields "23,5,35/SEARCH 28,X,2,2;2,2,LIST 20/25,ROWSUB,SELONE,1005" : selectedfunction$,newselection
36948         else 
36949           input #window, fields "23,5,35/SEARCH 28,X,2,2;2,2,LIST 20/25,ROWSUB,SELONE,-1" : selectedfunction$,newselection
36950         end if 
36951       else 
36952         rinput #window, fields "23,5,35/C 28,X" : selectedfunction$
36953       end if 
36954 ! 
36955       let selectedfunction$=trim$(selectedfunction$)(1:28)
36956       for index=1 to len(selectedfunction$)
36957         if ~pos("abcdefghijklmnopqrstuvwxyz0123456789_",lwrc$(selectedfunction$(index:index))) then 
36958           let selectedfunction$(index:index)="_"
36959         end if 
36960       next index
36961 ! 
36962       if currentfield=1 then ! We Are In The Search Box
36963         if fkey=15 then ! Up
36964           let currentfield=2
36965           let tempselection=srch(mat lvsf_fnfl$,lwrc$(trim$(selectedfunction$)))
36966           if tempselection>0 then let newselection=tempselection
36967         end if 
36968         if fkey=16 then ! Dn
36969           let currentfield=2
36970           let tempselection=srch(mat lvsf_fnfl$,lwrc$(trim$(selectedfunction$)))
36971           if tempselection>0 then let newselection=tempselection
36972         end if 
36973       else ! We Are In The Listview
36974         if fkey=15 then ! Up
36975           if newselection<=1 then let currentfield=1 else let newselection-=1
36976         end if 
36977         if fkey=16 then ! Dn
36978           if newselection=>udim(lvsf_fnfl$) then let currentfield=1 else let newselection+=1
36979         end if 
36980       end if 
36981       if fkey=17 or fkey=200 then ! Tab Or Shift Tab Or Click On Other Control
36982         if currentfield=1 then let currentfield=2 else let currentfield=1
36983         if currentfield=2 then 
36984           if fn42 then input #window, fields "2,2,LIST 20/25,ROWSUB,NEXT,NOWAIT" : newselection
36985         end if 
36986       end if 
36987       if fkey=1005 then ! Listview Clicked On
36988         let currentfield=2
36989         if fn42 then input #window, fields "2,2,LIST 20/25,ROWSUB,NEXT,NOWAIT" : newselection
36990       end if 
36991       if fkey=1004 then ! Search Box Clicked On
36992         let currentfield=1
36993       end if 
36994       if fkey=34 then ! New Button Clicked On
36995         if len(trim$(selectedfunction$)) then 
36996           if 2=msgbox("Do you wish to create "&selectedfunction$&".brs","Proceed?","yN","QST") then 
36997             let fncreatenewfunction(selectedfunction$,functiontype,subtype)
36998             let fneditcustomfunction(selectedfunction$)
36999             let fnreadavailablefunctions(mat lvsf_fnfl$)
37000             let currentfield=2
37001             let newselection=srch(mat lvsf_fnfl$,lwrc$(trim$(selectedfunction$)))
37002             if newselection>0 then let fkey(35) ! New Button Also Selects
37003             print #window, fields "2,2,LIST 20/25,=" : (mat lvsf_fnfl$)
37004           end if 
37005         else 
37006           let msgbox("Please type a name for your new function.","No Name Given")
37007         end if 
37008       end if 
37009       if fkey=37 then 
37010         dim selfunction(1)
37011         dim selfunction$(1)*255
37012 ! 
37013         let selfunction(1)=1
37014         let selfunction$(1)="{"&lvsf_fnfl$(newselection)&"}"
37015         let done=fnceaffectedscreenlist(mat selfunction,mat selfunction$)
37016         let fkey(-1) ! Clear fkey
37017       end if 
37018     loop until fkey=99 or fkey=35 or ((fkey>1100) and (fkey<1699)) or fkey=98 or fkey=93 or done
37019 ! 
37020     execute "config keyboard clear"
37021 ! 
37022     if fkey=35 then 
37023       let selectedfunction$=lvsf_fnfl$(newselection)
37024       let selectedfunction$="{"&trim$(selectedfunction$)&"}"
37025     else 
37026       let selectedfunction$=current$
37027     end if 
37028     if fkey<>99 and fkey<>35 then 
37029       let returnfkey=fkey
37030     end if 
37031     if done then 
37032       let returnfkey=99
37033     end if 
37034 ! 
37035     let fnselectfunctiondialog$=selectedfunction$
37036 ! 
37037     if textboxwindow and file(textboxwindow)>-1 then close #textboxwindow: 
37038     if window and file(window)>-1 then close #window: 
37039 ! 
37040   fnend 
37041 ! 
37042 ! *****************************************************************
37043 ! ******************* Validate And Render Screen ******************
37044 ! *****************************************************************
37045 VALIDATEFIELDS: !  Validate All Data And Populate Todo Listview
37046   def fnvalidatefields(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,index,mode,object,message$*255,jndex,goodfunk,funk$*255,lvcount,badlv)
37047 ! 
37048     let fncleardebuglistview
37049 ! 
37050 ! .       !  Validate Header Fields
37051     if trim$(screenio$(si_screencode))="" then 
37052       let fnprinttodebuglistview(inputattributesmode,si_screencode,"Window Name cannot be blank.",db_error)
37053     end if 
37054 ! 
37055     if trim$(screenio$(si_picture))<>"" and ~exists(screenio$(si_picture)) then 
37056       let fnprinttodebuglistview(inputattributesmode,si_picture,"Picture does not exist.",db_error)
37057     end if 
37058 ! 
37059     if trim$(screenio$(si_filelay))="" then 
37060       let fnprinttodebuglistview(selectfilelaymode,si_filelay,"You should enter a file layout.",db_warning)
37061     end if 
37062 ! 
37063     if trim$(screenio$(si_filelay))<>"" and ~fndoeslayoutexist(screenio$(si_filelay)) then 
37064       let fnprinttodebuglistview(selectfilelaymode,si_filelay,"File layout not found.",db_error)
37065     end if 
37066 ! 
37067     let lvcount=badlv=0
37068     for index=1 to udim(mat controlname$)
37069       if lwrc$(trim$(fieldtype$(index)))="listview" then 
37070         let lvcount+=1
37071         let badlv=index
37072       end if 
37073     next index
37074 ! 
37075     if lvcount>1 then 
37076       let fnprinttodebuglistview(inputeditormode,badlv,"You have 2 listviews. One of them will be inactive when running your screen.",db_warning)
37077     end if 
37078 ! 
37079     for index=1 to udim(mat controlname$)
37080       if lwrc$(trim$(fieldtype$(index)))<>"listchld" then 
37081         let mode=1
37082         if ~fnvalidspec(index,mat screenio$,mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mode,object,message$) then 
37083           let fnprinttodebuglistview(mode,object,message$,db_error)
37084         end if 
37085         if ~fnvalidattribute(attr$(index),fieldtype$(index)) then 
37086           let fnprinttodebuglistview(inputeditormode,index,"Invalid attribute Specified for Control "&str$(index)&".",db_error)
37087         end if 
37088       end if 
37089 ! 
37090       if function$(index)<>"" and function$(index)(1:1)<>"{" and function$(index)(1:1)<>"[" and function$(index)(1:1)<>"%" and function$(index)(1:1)<>"#" then 
37091         let funk$=trim$(function$(index))
37092         let goodfunk=0
37093         for jndex=1 to len(funk$)
37094           if ~pos("abcdefghijklmnopqrstuvwxyz",lwrc$(funk$(jndex:jndex))) then let goodfunk=1
37095         next jndex
37096         if ~goodfunk then 
37097           let fnprinttodebuglistview(inputeditormode,index,"Control "&str$(index)&"'s Function doesn't look right. It doesn't do anything.",db_warning)
37098         end if 
37099       end if 
37100 ! 
37101       if trim$(controlname$(index))="" and trim$(fieldname$(index))="" then 
37102         if lwrc$(trim$(fieldtype$(index))) = "listchld" or lwrc$(trim$(fieldtype$(index))) = "c" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "listchld" # "c"
37103           let fnprinttodebuglistview(inputeditormode,index,"Control "&str$(index)&" has no source. It needs either a Control Name or a Field Name.",db_warning)
37104         else if lwrc$(trim$(fieldtype$(index))) = "button" or lwrc$(trim$(fieldtype$(index))) = "caption" or lwrc$(trim$(fieldtype$(index))) = "picture" then ! #Case# "button" # "caption" # "picture"
37105           if trim$(description$(index))="" then 
37106             let fnprinttodebuglistview(inputeditormode,index,"Control "&str$(index)&" has no source. It needs either a Control Name or a Caption.",db_warning)
37107           end if 
37108         end if  ! #End Select#
37109       end if 
37110 ! 
37111       if lwrc$(trim$(fieldtype$(index))) = "c" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "c"
37112         if trim$(fieldname$(index))="" then 
37113 !                Let Fnprinttodebuglistview(Inputeditormode,Index,"Control "&Str$(Index)&" is not tied to a field.",Db_Warning)
37114         end if 
37115 ! 
37116       else if lwrc$(trim$(fieldtype$(index))) = "button" then ! #Case# "button"
37117         if trim$(function$(index))="" then 
37118           let fnprinttodebuglistview(inputeditormode,index,"Control "&str$(index)&" has no function.",db_warning)
37119         end if 
37120 ! 
37121       else if lwrc$(trim$(fieldtype$(index))) = "p" then ! #Case# "p"
37122         if trim$(picture$(index))="" then 
37123 !              Let Fnprinttodebuglistview(Inputeditormode,Index,"Control "&Str$(Index)&" has no picture assigned.",Db_Warning)
37124         else if ~exists(trim$(picture$(index))) then 
37125           let fnprinttodebuglistview(inputeditormode,index,"Picture File "&trim$(picture$(index))&" could not be found.",db_error)
37126         end if 
37127 ! 
37128       else if lwrc$(trim$(fieldtype$(index))) = "calendar" then ! #Case# "calendar"
37129 ! .         ! $$$$$  Give a warning if its not tied to any text box
37130 ! 
37131 ! 
37132       else if lwrc$(trim$(fieldtype$(index))) = "caption" then ! #Case# "caption"
37133         if trim$(description$(index))="" then 
37134 !               Let Fnprinttodebuglistview(Inputeditormode,Index,"Control "&Str$(Index)&" has no caption set.",Db_Warning)
37135         end if 
37136 ! 
37137       else if lwrc$(trim$(fieldtype$(index))) = "listview" then ! #Case# "listview"
37138         if ~fncountcolumns(parent$(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
37139           let fnprinttodebuglistview(inputlistviewmode,index,"Listview "&str$(index)&" has no columns.",db_error)
37140         end if 
37141 ! 
37142       else if lwrc$(trim$(fieldtype$(index))) = "search" then ! #Case# "search"
37143         if ~(target:=fnfindtarget(parent$(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)) then 
37144           let fnprinttodebuglistview(inputeditormode,index,"Search Box "&str$(index)&" is not tied to a listview.",db_error)
37145         else if specwidth(target)=0 then 
37146           let fnprinttodebuglistview(inputeditormode,target,"Listview "&str$(index)&" has a Search Box but no Sort Column.",db_warning)
37147         end if 
37148 ! 
37149       else if lwrc$(trim$(fieldtype$(index))) = "filter" then ! #Case# "filter"
37150         if ~(target:=fnfindtarget(parent$(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)) then 
37151           let fnprinttodebuglistview(inputeditormode,index,"Filter Box "&str$(index)&" is not tied to a listview.",db_error)
37152         else 
37153           let fncleanfilter(picture$(index))
37154           if lwrc$(picture$(index))(1:4)><"full" then 
37155             if val(picture$(index)(1:pos(picture$(index),",")-1))>fncountcolumns(parent$(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
37156               let fnprinttodebuglistview(inputeditormode,index,"Filter Box "&str$(index)&" is tied to a column that doesn't exist.",db_error)
37157             end if 
37158           end if 
37159         end if 
37160 ! 
37161       else if lwrc$(trim$(fieldtype$(index))) = "screen" then ! #Case# "screen"
37162         if lwrc$(trim$(fieldname$(index)))=lwrc$(trim$(screenio$(si_screencode))) then 
37163           let fnprinttodebuglistview(inputeditormovemode,index,"It's probably not a good idea to place a screen on top of itself.",db_error)
37164         end if 
37165       end if  ! #End Select#
37166     next index
37167     let fnvalidatefields=1 !   True For Now
37168 ! 
37169   fnend 
37170 ! 
37171 RENDERSCREEN: ! Draw All Valid Controls On The Screen
37172   def fnrenderscreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;dontclear,___,index,window,arrayindex)
37173     dim renderspec$(1)*255, renderdata$(1)*255
37174 ! 
37175     mat renderspec$(0)
37176     mat renderdata$(0)
37177 ! 
37178     let window=fngeteditorwindow
37179     let fndrawframes(window,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
37180     let fndrawscreens(window,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
37181 ! 
37182     for index=1 to udim(mat controlname$)
37183       if ((lwrc$(trim$(fieldtype$(index)))<>"listview") and (lwrc$(trim$(fieldtype$(index)))<>"listchld") and (lwrc$(trim$(fieldtype$(index)))<>"frame") and (lwrc$(trim$(fieldtype$(index)))<>"screen")) then 
37184         if fnvalidspec(index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
37185           let arrayindex=udim(mat renderspec$)+1
37186           mat renderdata$(arrayindex)
37187           mat renderspec$(arrayindex)
37188           let renderdata$(arrayindex)=fncalculatedata$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
37189           let renderspec$(arrayindex)=fncalculatespec$(window,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,1)
37190         end if 
37191       end if 
37192     next index
37193 ! 
37194     let fnchangeforcevisibility(1)
37195     if ~dontclear then 
37196       print #window: newpage
37197       let dotsprinted=0 ! Dots were erased
37198       let settingtempdots=0
37199     end if 
37200     print #window, fields mat renderspec$ : mat renderdata$
37201     let fnchangeforcevisibility(0)
37202 ! 
37203     let fndrawalllistviews(window,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
37204     let fntrytopopulatelistviews(window,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
37205   fnend 
37206 ! 
37207 VALIDSPEC: ! Returns True If A Spec Is Valid, Sets Object, Control, Message If Not
37208   def fnvalidspec(control,mat screenio$,mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;&mode,&object,&message$,_,index,valid,testrow, testcol,framecontrol,othercontrol,inside,setinside)
37209     let valid=1 !  Spec Starts Out Valid
37210     if hposition(control)+(width(control)+(2*(lwrc$(trim$(fieldtype$(control)))=="combo"))-1)>screenio(si_hsize) then 
37211       let valid=0 ! Not Valid
37212       if mode then 
37213         let mode=inputeditormode
37214         let object=control
37215         let message$="Control is outside right boundary of Window."
37216       end if 
37217     end if 
37218     if vposition(control)+(max(height(control),1)-1)>screenio(si_vsize) then 
37219       let valid=0
37220       if mode then 
37221         let mode=inputeditormode
37222         let object=control
37223         let message$="Control is outside bottom boundary of Window."
37224       end if 
37225     end if 
37226     if valid then ! If Still Valid
37227       if lwrc$(trim$(fieldtype$(control)))<>"listchld" and lwrc$(trim$(fieldtype$(control)))<>"screen" then 
37228         for index=1 to control-1
37229           let setinside=0
37230           let inside=0
37231           if lwrc$(trim$(fieldtype$(index)))<>"listchld" and lwrc$(trim$(fieldtype$(control)))<>"screen" then 
37232             if lwrc$(trim$(fieldtype$(control)))="frame" and lwrc$(trim$(fieldtype$(index)))="listview" or lwrc$(trim$(fieldtype$(index)))="frame" and lwrc$(trim$(fieldtype$(control)))="listview" or lwrc$(trim$(fieldtype$(control)))<>"frame" and lwrc$(trim$(fieldtype$(index)))<>"frame" then 
37233               for testrow=vposition(control) to vposition(control)+max(1,height(control))-1
37234                 for testcol=hposition(control) to hposition(control)+width(control)+(2*(lwrc$(trim$(fieldtype$(control)))=="combo"))-1
37235                   if fncontrolisinside(testrow,testcol,vposition(index),hposition(index),width(index)+(2*(lwrc$(trim$(fieldtype$(index)))=="combo")),height(index)) then 
37236                     let valid=0
37237                     if mode then 
37238                       let mode=inputeditormovemode
37239                       let object=control
37240                       if lwrc$(trim$(fieldtype$(control)))<>"frame" and lwrc$(trim$(fieldtype$(index)))<>"frame" then 
37241                         let message$="Control "&str$(control)&" overlaps control "&str$(index) &"."
37242                       else 
37243                         let message$="BR doesn't support a listview on a frame."
37244                       end if 
37245                     end if 
37246                     let testcol=hposition(control)+width(control)+(2*(lwrc$(trim$(fieldtype$(control)))=="combo"))-1
37247                     let testrow=vposition(control)+max(1,height)-1
37248                     goto ENDTEST ! We Found A Problem, We Can Stop Searching
37249                   end if 
37250                 next testcol
37251               next testrow
37252             else if lwrc$(trim$(fieldtype$(control)))="frame" or lwrc$(trim$(fieldtype$(index)))="frame" then 
37253               if lwrc$(trim$(fieldtype$(control)))="frame" then 
37254                 let framecontrol=control
37255                 let othercontrol=index
37256               else 
37257                 let framecontrol=index
37258                 let othercontrol=control
37259               end if 
37260               for testrow=vposition(othercontrol) to vposition(othercontrol)+max(1,height(othercontrol))-1
37261                 for testcol=hposition(othercontrol) to hposition(othercontrol)+width(othercontrol)+(2*(lwrc$(trim$(fieldtype$(othercontrol)))=="combo"))-1
37262                   if ~setinside then 
37263                     let inside=fncontrolisinside(testrow,testcol,vposition(framecontrol),hposition(framecontrol),width(framecontrol),height(framecontrol))
37264                     let setinside=1
37265                   else 
37266                     if inside<>fncontrolisinside(testrow,testcol,vposition(framecontrol),hposition(framecontrol),width(framecontrol),height(framecontrol)) then 
37267                       let valid=0
37268                       if mode then 
37269                         let mode=inputeditormovemode
37270                         let object=othercontrol
37271                         let message$="Control "&str$(othercontrol)&" is halfway outside of a frame."
37272                       end if 
37273                     end if 
37274                   end if 
37275                 next testcol
37276               next testrow
37277             end if 
37278           end if 
37279         next index
37280       end if 
37281     end if 
37282 ENDTEST: ! End Of The Test Here
37283     let fnvalidspec=valid
37284   fnend 
37285 ! 
37286 ! 
37287   dim validspecattributes$(1)*128
37288   dim validspecattributes(1)
37289 ! 
37290   def fnvalidattribute(attribute$*128,fieldtype$;___,visibility)
37291     if len(trim$(attribute$)) then 
37292       if udim(mat validspecattributes$)=1 and validspecattributes$(1)="" then 
37293         mat validspecattributes$(0)
37294         mat validspecattributes(0)
37295       end if 
37296       let i=srch(mat validspecattributes$,attribute$)
37297       if i<1 then 
37298         let i=udim(mat validspecattributes$)+1
37299         mat validspecattributes$(i)
37300         mat validspecattributes(i)
37301         let validspecattributes$(i)=attribute$
37302 ! 
37303         let visibility=fnforcevisibility
37304         let fnchangeforcevisibility(0)
37305         if lwrc$(trim$(fieldtype$))="check" and pos("0123456789",attribute$(1:1)) then 
37306           let attribute$(1:1)="" ! strip the number thats breaking design
37307         end if 
37308         print #0, fields "3,3,C 1,"&attribute$ : "" error _INVALIDATTRIBUTE
37309         let validspecattributes(i)=1
37310 _INVALIDATTRIBUTE: ! Invalid Attribute
37311         let fnchangeforcevisibility(visibility)
37312       end if 
37313       let fnvalidattribute=validspecattributes(i)
37314     else 
37315       let fnvalidattribute=1
37316     end if 
37317   fnend 
37318 ! 
37319   dim validwindowattributes$(1)*128
37320   dim validwindowattributes(1)
37321 ! 
37322   def fnvalidframe(attribute$*128;___,x,i)
37323     if len(trim$(attribute$)) then 
37324       if udim(mat validwindowattributes$)=1 and validwindowattributes$(1)="" then 
37325         mat validwindowattributes$(0)
37326         mat validwindowattributes(0)
37327       end if 
37328       let i=srch(mat validwindowattributes$,attribute$)
37329       if i<1 then 
37330         let i=udim(mat validwindowattributes$)+1
37331         mat validwindowattributes$(i)
37332         mat validwindowattributes(i)
37333         let validwindowattributes$(i)=attribute$
37334         open #(x:=fngetfilenumber) : "srow=2,scol=2,rows=1,cols=1"&attribute$,display,outin error _INVALIDFRAME
37335         close #x: 
37336         let validwindowattributes(i)=1
37337 _INVALIDFRAME: ! Invalid Frame
37338       end if 
37339       let fnvalidframe=validwindowattributes(i)
37340     else 
37341       let fnvalidframe=1
37342     end if 
37343   fnend 
37344 ! 
37345 CONTROLISINSIDE: ! Return True If The Given Position Is Inside The Given Control Boundaries
37346   def fncontrolisinside(testrow,testcol,row,col,width,height)
37347     if height=0 then let height=1
37348     if testrow=>row and testrow<=row+height-1 and testcol=>col and testcol<=col+width-1 then 
37349       let fncontrolisinside=1
37350     else 
37351       let fncontrolisinside=0
37352     end if 
37353   fnend 
37354 ! 
39000 !  #Autonumber# 39000,1
39001 ! *****************************************************************
39002 ! ******************** Add New Controls ***************************
39003 ! *****************************************************************
39004   dim gaps(1)
39005   def fnfindavailablevposition(;&hpos,&width,___,index,row,found)
39006     mat gaps(screenio(si_vsize))
39007     if ~hpos then let hpos=int(screenio(si_hsize)/4)
39008     if ~width then let width=18
39009     let row=1
39010     do while row<screenio(si_vsize)-1
39011       let row+=1
39012       if ~gaps(row) then 
39013         let found=fncheckrow(row,hpos,width,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39014       end if 
39015     loop until found
39016     if ~found then ! Screen Needs To Be Made Bigger
39017       let screenio(si_vsize)+=1
39018       let found=screenio(si_vsize)-1
39019     end if 
39020     let fnfindavailablevposition=found
39021   fnend 
39022 ! 
39023   def fncheckrow(row,hpos,width,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,found,testcol)
39024     do while index<udim(mat controlname$)
39025       let index+=1
39026       for testcol=hpos to hpos+width
39027         if lwrc$(trim$(fieldtype$(index)))="frame" then 
39028           if gridlines(index) then 
39029             if fncontrolisinside(row,testcol,vposition(index)-1,hposition(index),width(index),1) then let found=1
39030             if fncontrolisinside(row,testcol,vposition(index)+height(index),hposition(index),width(index),1) then let found=1
39031           end if 
39032         else 
39033           if fncontrolisinside(row,testcol,vposition(index),hposition(index),width(index),height(index)) then let found=1
39034         end if 
39035       next testcol
39036     loop until found
39037     if ~found then 
39038       let fncheckrow=row
39039     end if 
39040   fnend 
39041 ! 
39042   def fnskipagap(;___,x)
39043     let x=fnfindavailablevposition
39044     if x then let gaps(x)=1
39045   fnend 
39046 ! 
39047   def fnresizescreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,rightedge,bottomedge)
39048     for index=1 to udim(mat hposition)
39049       let rightedge=max(rightedge,hposition(index)+width(index)-1)
39050       let bottomedge=max(bottomedge,vposition(index)+height(index)-1)
39051     next index
39052 ! 
39053     let screenio(si_vsize)=max(5,bottomedge+1)
39054     let screenio(si_hsize)=max(30,rightedge+4)
39055   fnend 
39056 ! 
39057   def fntheresalistview(mat fieldtype$;___,index,listview)
39058     for index=1 to udim(mat fieldtype$)
39059       if lwrc$(trim$(fieldtype$(index)))="listview" then let listview=1
39060     next index
39061     let fntheresalistview=listview
39062   fnend 
39063 ! 
39064   def fnfindexitrow(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,foundexitrow)
39065     for index=1 to udim(mat controlname$)
39066       if controlname$(index)="Cancel_Button" or controlname$(index)="Save_Button" or controlname$(index)="Select_Button" then 
39067         let foundexitrow=index
39068       end if 
39069       if controlname$(index)="Add_Button" or controlname$(index)="Edit_Button" then 
39070         let foundexitrow=index
39071       end if 
39072     next index
39073     if foundexitrow=0 then 
39074       for index=1 to udim(mat function$)
39075         if pos(lwrc$(function$(index)),"exitmode") and pos(lwrc$(function$(index)),"quit") then 
39076           let foundexitrow=index
39077         end if 
39078       next index
39079     end if 
39080     if foundexitrow then 
39081       let fnfindexitrow=vposition(foundexitrow)
39082     end if 
39083   fnend 
39084 ! 
39085   dim screens$(1)
39086   def fnaddaebuttons(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;target$,row,col,noadd,noedit,nodelete,&add,&edit,&deletebutton,___,index,findlistview,column)
39087     if target$="" then 
39088 ! .      ! library "fileio" : fnAskCombo$ Linkage already established
39089       let fnreadallscreencodes(mat tempscreenlist$,mat tempcreatedate,mat tempmodifydate)
39090 ! 
39091       let target$=fnaskcombo$(mat tempscreenlist$,"Select target:")
39092     end if 
39093 ! 
39094     for index=1 to udim(mat fieldtype$)
39095       if lwrc$(trim$(fieldtype$))="listview" then 
39096         let findlistview=index
39097       end if 
39098     next index
39099 ! 
39100     if ~row then let row=fnfindexitrow(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39101     if ~row then let fnskipagap
39102 ! 
39103     if ~col then 
39104       if findlistview then 
39105         let col=hposition(findlistview)
39106       else 
39107         let col=10
39108       end if 
39109     end if 
39110 ! 
39111     if ~noadd then 
39112       let add=fnaddbutton(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39113       if add then 
39114         if ~row then let row=vposition(add)
39115         let width(add)=10
39116         let vposition(add)=row
39117         let hposition(add)=col
39118         let controlname$(add)="Add_Button"
39119         let description$(add)="Add"
39120         let function$(add)="["&uprc$(target$)&"]"
39121         let fgcolor$(add)="W"
39122         let bgcolor$(add)="W"
39123         let col+=12 ! Move over for Next Button
39124       end if 
39125     end if 
39126 ! 
39127     if ~noedit then 
39128       let edit=fnaddbutton(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39129       if edit then 
39130         let width(edit)=10
39131         let vposition(edit)=row
39132         let hposition(edit)=col
39133         let controlname$(edit)="Edit_Button"
39134         let description$(edit)="Edit"
39135         let function$(edit)="["&uprc$(target$)&"]Record=CurrentRec"
39136         let fgcolor$(edit)="W"
39137         let bgcolor$(edit)="W"
39138         let col+=12 ! Move over for Next Button
39139       end if 
39140     end if 
39141 ! 
39142     if ~nodelete then 
39143 ! .      ! Add Delete Button
39144       let deletebutton=fnaddbutton(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39145       if deletebutton then 
39146         let width(deletebutton)=10
39147         let vposition(deletebutton)=row
39148         let hposition(deletebutton)=col
39149         let controlname$(deletebutton)="Delete_Button"
39150         let description$(deletebutton)="Delete"
39151         let function$(deletebutton)="{deletelistviewrecord}"
39152         let fgcolor$(deletebutton)="W"
39153         let bgcolor$(deletebutton)="W"
39154         let col+=12 ! Move over for Next Button
39155       end if 
39156     end if 
39157   fnend 
39158 ! 
39159   def fnaddexitbuttons(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;row,&cancelbutton,&okbutton,___,cancel,save,index,rightedge)
39160 ! .   ! IF there's AE buttons, use that row
39161 ! .   !  if not, use the skipagap logic
39162     if ~row then let row=fnfindexitrow(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39163     if ~row then let fnskipagap
39164 ! 
39165     for index=1 to udim(mat hposition)
39166       let rightedge=max(rightedge,hposition(index)+width(index)-1)
39167     next index
39168     if ~rightedge then let rightedge=screenio(si_hsize)-4
39169     let rightedge=max(rightedge,24)
39170 ! 
39171     let cancel=fnaddbutton(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39172     if ~row then let row=vposition(cancel)
39173     let vposition(cancel)=row
39174     let width(cancel)=10
39175     let hposition(cancel)=rightedge-width(cancel)+1
39176     let controlname$(cancel)="Cancel_Button"
39177     let description$(cancel)="Cancel"
39178     let function$(cancel)="let ExitMode=QuitOnly"
39179     let fgcolor$(cancel)="W"
39180     let bgcolor$(cancel)="W"
39181 ! 
39182     let save=fnaddbutton(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39183     let width(save)=10
39184     let vposition(save)=row
39185     let hposition(save)=hposition(cancel)-2-width(save)
39186     let fgcolor$(save)="W"
39187     let bgcolor$(save)="W"
39188 ! 
39189     if fntheresalistview(mat fieldtype$) then 
39190       let controlname$(save)="Select_Button"
39191       let description$(save)="Select"
39192       let function$(save)="let ExitMode=SelectAndQuit"
39193     else 
39194       let controlname$(save)="Save_Button"
39195       let description$(save)="Save"
39196       let function$(save)="let ExitMode=SaveAndQuit"
39197     end if 
39198     let fnaddexitbuttons=save
39199 ! 
39200     let cancelbutton=cancel
39201     let okbutton=save
39202 ! 
39203 ! .   ! let Description$(Save)="Ok" ! Uncomment this line to make it use OK for the save button.
39204   fnend 
39205 ! 
39206 ADDEMPTYFIELD: ! Add An Empty Field To The Editor Arrays
39207   def fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;skipplacement,specifyrow,___,index)
39208     let fnresizecontrolarrays(index:=udim(controlname$)+ 1,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39209     let width(index)=18
39210     if specifyrow then 
39211       let vposition(index)=specifyrow
39212     else if ~skipplacement then 
39213       let vposition(index)=fnfindavailablevposition(hposition(index),width(index)) ! Sets Hposition To 1/4th The Screen Size Also
39214     else 
39215       let vposition(index)=1 ! in case it has to be something..
39216     end if 
39217     let height(index)=1
39218     let fnaddemptyfield=index
39219   fnend 
39220 ! 
39221   def fnaddandcolorcurrentfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat screenio$,mat screenio;name$*50,description$*255,type$*30,&captionindex,specifyrow,___,index,relatedcontrol)
39222     let index=fnaddcurrentfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,screenio(si_hsize),name$,description$,type$,captionindex,specifyrow)
39223     if index then 
39224       let fgcolor$(index)="W"
39225       let bgcolor$(index)="W"
39226       if trim$(screenio$(si_fgcolor))<>"" then 
39227         if (relatedcontrol:=srch(mat controlname$,description$(index)))>0 then 
39228           let fgcolor$(relatedcontrol)=trim$(screenio$(si_fgcolor))
39229         end if 
39230       end if 
39231     end if 
39232     let fnaddandcolorcurrentfield=index
39233   fnend 
39234 ! 
39235 ADDCURRENTFIELD: ! Add The Current Field To The Editor Arrays
39236   def fnaddcurrentfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,screenwidth;name$*50,description$*255,type$*30,&captionfield,specifyrow,___,index,captiondisplaywidth,vpos,hpos,nofieldsel)
39237     if name$="" and description$="" and type$="" then 
39238       if fngetfieldscount then 
39239         if ~fnreadfields(name$,description$,type$) then 
39240           let nofieldsel=1 ! Abort
39241         end if 
39242       else 
39243         let nofieldsel=1
39244       end if 
39245     end if 
39246     if ~nofieldsel then 
39247       if len(trim$(description$)) then let description$=description$&": "
39248       let captiondisplaywidth=int(len(trim$(description$))*4/5)+2
39249       let index=fnaddtextfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,specifyrow)
39250       let fieldtype$(index)="C"
39251       let controlname$(index)="Field_"&trim$(name$)(1:44)
39252       let description$(index)="Caption_"&trim$(name$)(1:42)
39253       let fieldname$(index)=trim$(name$)
39254       let width(index)=fndisplaylength(type$)
39255       let specwidth(index)=width(index)
39256       let width(index)=min(width(index),40)
39257       let hposition(index)=max(captiondisplaywidth+2,hposition(index))
39258       let width(index)=min(width(index),screenwidth-hposition(index))
39259       let vpos=vposition(index)
39260       let hpos=hposition(index)
39261 ! 
39262       let fnaddcurrentfield=index
39263       let index=fnaddcaption(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39264       let fieldtype$(index)="caption"
39265       let controlname$(index)="Caption_"&trim$(name$)(1:42)
39266       let description$(index)=trim$(description$)
39267       let justify$(index)="R"
39268       let vposition(index)=vpos
39269       let hposition(index)=hpos-(captiondisplaywidth+1)
39270       let width(index)=captiondisplaywidth
39271       let specwidth(index)=len(trim$(description$))
39272       let captionfield=index
39273     end if 
39274   fnend 
39275 ! 
39276 ADDTEXTFIELD: ! Add A  Textbox Field
39277   def fnaddtextfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;specifyrow,___,index)
39278     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,specifyrow)
39279     let fieldtype$(index)="C"
39280     let specwidth(index)=width(index)
39281     let parent$(index)=fngenerateuniquelvname$(mat parent$,"Textbox")
39282     let fnaddtextfield=index
39283   fnend 
39284 ! 
39285 ADDCAPTION: ! Add A Caption Field
39286   def fnaddcaption(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
39287     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39288     let fieldtype$(index)="caption"
39289     let specwidth(index)=width(index)
39290     let parent$(index)=fngenerateuniquelvname$(mat parent$,"Caption")
39291     let fnaddcaption=index
39292   fnend 
39293 ! 
39294 ADDSCREEN: ! Add An Entire Screen As A Clickable Field
39295   def fnaddscreen(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,screen$)
39296     let screen$=fnselectscreen$
39297     if trim$(screen$)<>"" then 
39298       let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39299       let fieldtype$(index)="screen"
39300       let fieldname$(index)=screen$
39301       let parent$(index)=fngenerateuniquelvname$(mat parent$,"Screen")
39302 ! 
39303       let fnreadchildscreensize(index,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39304 ! 
39305       let function$(index)="ParentKey$=CurrentKey$"
39306       let fnaddscreen=index
39307     end if 
39308   fnend 
39309 ! 
39310 ADDBUTTON: ! Add A Button
39311   def fnaddbutton(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
39312     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39313     let fieldtype$(index)="button"
39314     let specwidth(index)=width(index)
39315     let parent$(index)=fngenerateuniquelvname$(mat parent$,"Button")
39316     let fnaddbutton=index
39317   fnend 
39318 ! 
39319 ADDCOMBO: ! Add a Combo Box to the Screen
39320   def fnaddcombo(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
39321     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39322     let fieldtype$(index)="combo"
39323     let specwidth(index)=width(index)
39324     let fnaddcombo=index
39325   fnend 
39326 ! 
39327 ADDCHECKBOX: ! Add A Checkbox To The Screen
39328   def fnaddcheckbox(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
39329     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39330     let fieldtype$(index)="check"
39331     let truevalue$(index)="1"
39332     let falsevalue$(index)="0"
39333     let specwidth(index)=width(index)
39334     let fnaddcheckbox=index
39335   fnend 
39336 ! 
39337 ADDPICTURE: ! Add A Picture
39338   def fnaddpicture(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
39339     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39340     let fieldtype$(index)="P"
39341     let width(index)=10
39342     let height(index)=5
39343     let fnaddpicture=index
39344   fnend 
39345 ! 
39346 ADDFRAME: ! Add A Picture
39347   def fnaddframe(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
39348     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39349     let fieldtype$(index)="frame"
39350     let gridlines(index)=1
39351     let parent$(index)=fngenerateuniquelvname$(mat parent$,"Frame")
39352     let width(index)=40
39353     let height(index)=5
39354     let fnaddframe=index
39355   fnend 
39356 ! 
39357 ADDLISTVIEW: ! Add A Listview
39358   def fnaddlistview(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;skipemptychild,___,index)
39359     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39360     let fieldtype$(index)="LISTVIEW"
39361     let width(index)=40
39362     let height(index)=11
39363     let parent$(index)=fngenerateuniquelvname$(mat parent$)
39364     let fnaddlistview=index
39365 ! 
39366     if ~skipemptychild then 
39367       let index=fnaddlistviewchild(parent$(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39368       let description$(index)="Empty Listview"
39369       let width(index)=40
39370     end if 
39371   fnend 
39372 ! 
39373 ADDLISTVIEWCHILD: ! Add A Listview Column
39374   def fnaddlistviewchild(parent$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
39375     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
39376     let fieldtype$(index)="LISTCHLD"
39377     let width(index)=10
39378     let parent$(index)=parent$
39379     let fnaddlistviewchild=index
39380   fnend 
39381 ! 
39382 ADDSEARCHBOX: ! Add A Searchbox Field
39383   def fnaddsearchbox(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;mode,control,___,index,listviewindex,captionindex)
39384 ! 
39385     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
39386     let fieldtype$(index)="SEARCH"
39387     let specwidth(index)=width(index)
39388     let hposition(index)=3
39389 ! 
39390     if (mode=inputeditormode or mode=inputeditormovemode or mode=inputlistviewmode) and fieldtype$(control)="LISTVIEW" then 
39391       let parent$(index)=parent$(control)
39392       let fnsetsearchlistview(index,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
39393     else 
39394       let listviewindex=srch(mat fieldtype$,"LISTVIEW")
39395       if listviewindex>0 then 
39396         let fnsetsearchlistview(index,listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
39397       else 
39398         let vposition(index)=fnfindavailablevposition(hposition(index),width(index))
39399       end if 
39400     end if 
39401 ! 
39402     let captionindex=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
39403     let fieldtype$(captionindex)="P"
39404     let picture$(captionindex)=setting_imagepath$&"\search.png"
39405     let width(captionindex)=2
39406     let height(captionindex)=1
39407     let hposition(captionindex)=hposition(index)-2
39408     let vposition(captionindex)=vposition(index)
39409 ! 
39410     let controlname$(captionindex)=fngenerateuniquelvname$(mat controlname$,"SearchPic_")
39411     let description$(index)=controlname$(captionindex)
39412 ! 
39413     let fnaddsearchbox=index
39414   fnend 
39415 ! 
39416 ADDFILTERBOX: ! Add A Filter Box Field
39417   def fnaddfilterbox(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;mode,control,___,index,listviewindex,captionindex)
39418 ! 
39419     if fn43 then 
39420 ! .      ! Add field, make it a filter
39421       let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
39422       let fieldtype$(index)="FILTER"
39423       let specwidth(index)=width(index)
39424       let hposition(index)=3
39425 ! .!
39426 ! .      ! Find listview
39427       if (mode=inputeditormode or mode=inputeditormovemode or mode=inputlistviewmode) and fieldtype$(control)="LISTVIEW" then 
39428         let parent$(index)=parent$(control)
39429         let fnsetsearchlistview(index,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
39430       else 
39431         let listviewindex=srch(mat fieldtype$,"LISTVIEW")
39432         if listviewindex>0 then 
39433           let fnsetsearchlistview(index,listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
39434         else 
39435           let vposition(index)=fnfindavailablevposition(hposition(index),width(index))
39436         end if 
39437       end if 
39438 ! .!
39439 ! .      ! Set default parameters
39440       let picture$(index)="0,ALL" ! Column, Filter Type, Case
39441 ! 
39442 ! .      ! Add magnifying glass icon
39443       let captionindex=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
39444       let fieldtype$(captionindex)="P"
39445       let picture$(captionindex)=setting_imagepath$&"\search.png"
39446       let width(captionindex)=2
39447       let height(captionindex)=1
39448       let hposition(captionindex)=hposition(index)-2
39449       let vposition(captionindex)=vposition(index)
39450 ! .!
39451 ! .      ! Tie magnifying glass icon to Filter Box so they move together
39452       let controlname$(captionindex)=fngenerateuniquelvname$(mat controlname$,"SearchPic_")
39453       let description$(index)=controlname$(captionindex)
39454 ! .!
39455       let fnaddfilterbox=index
39456     else 
39457       let msgbox("Filters are only available in BR 4.3 and higher.")
39458     end if 
39459   fnend 
39460 ! 
39461 ! 
39462 ADDCALENDAR: ! Add A Calendar
39463   def fnaddcalendar(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
39464     let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39465     let fieldtype$(index)="CALENDAR"
39466     let width(index)=10
39467     let height(index)=5
39468 ! 
39469 ! .   ! $$$$$ perhaps just preset function to be what it should be, like the shortcuts from the menu
39470 ! 
39471     let fnaddcalendar=index
39472   fnend 
39473 ! 
39474 ! 
39475 ADDCURRENTLISTCHLD: ! Add The Current Field As A Listview Child
39476   def fnaddcurrentlistchld(parent$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;name$*50,description$*255,type$*30,___,index,nofield)
39477     if name$="" and description$="" and type$="" then 
39478       if fngetfieldscount then 
39479         if ~fnreadfields(name$,description$,type$) then 
39480           let nofield=1
39481         end if 
39482       else 
39483         let nofield=1
39484       end if 
39485     end if 
39486     if ~nofield then 
39487       let index=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
39488       let fieldtype$(index)="LISTCHLD"
39489       let controlname$(index)="Field_"&trim$(name$)(1:44)
39490       let description$(index)=trim$(description$)
39491       let fieldname$(index)=trim$(name$)
39492       let specwidth(index)=fndisplaylength(type$)
39493       let width(index)=min(specwidth(index),40)
39494       let parent$(index)=parent$
39495       let fnaddcurrentlistchld=index
39496     end if 
39497   fnend 
39498 ! 
39499 GENERATEUNIQUELVNAME: ! Generate A Unique Name For A Lv
39500   def fngenerateuniquelvname$(mat parent$;prefix$,___,index,test)
39501     if ~len(prefix$) then let prefix$="LV"
39502     do 
39503     loop until (srch(mat parent$,prefix$&str$(index:=index+1))<1)
39504     let fngenerateuniquelvname$=prefix$&str$(index)
39505   fnend 
39506 ! 
39507 SETSEARCHLISTVIEW: ! Point The Search Box To A Listview Control
39508   def fnsetsearchlistview(index,listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;move,___,childindex)
39509     let parent$(index)=parent$(listviewindex)
39510     if move then 
39511       let width(index)=width(listviewindex)-2
39512       let hposition(index)=hposition(listviewindex)+2
39513       let vposition(index)=max(1,vposition(listviewindex)-1) ! Default Searchbox To Top
39514 ! .   !   let Vposition(Index)=Vposition(Listviewindex)+Height(listviewindex)
39515     end if 
39516     for childindex=1 to udim(mat fieldtype$)
39517       if fieldtype$(childindex)="LISTCHLD" and parent$(childindex)=parent$(listviewindex) then 
39518         let specwidth(index)=max(specwidth(index),specwidth(childindex))
39519       end if 
39520     next childindex
39521   fnend 
39522 ! 
39523   dim centerspec$(11)*64,centertext$(11)*64
39524 ! 
39525   def fnchangecontroltype(mat screenio$,mat screenio,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,key$,function,instr,index,fieldtype$,changed,_control,_type)
39526 ! 
39527     if ~fncontrolcanswitch(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then let control=0
39528 ! 
39529     for index=1 to 8
39530       let centerspec$(index)=str$(index)&",1,CC 24"
39531     next index
39532     let centerspec$(9)="10,1,24/C 40"
39533     let centerspec$(10)="11,1,24/C 40"
39534     let centerspec$(11)="13,7,CC 12,/W:W,B17"
39535 ! 
39536     mat centertext$=("")
39537     let centertext$(1)="Click on the Control"
39538     let centertext$(2)="that you want to change"
39539     let centertext$(3)=""
39540     let centertext$(4)="Then, click on the"
39541     let centertext$(5)="button in the toolbox"
39542     let centertext$(6)="for the control type"
39543     let centertext$(7)="you want to change it to"
39544     let centertext$(8)="or press ESC to cancel"
39545 ! 
39546     let centertext$(9)="Control:"
39547     let centertext$(10)="Type:"
39548 ! 
39549     let centertext$(11)="Convert"
39550 ! 
39551     let _control=9
39552     let _type=10
39553 ! 
39554     let fnchangeforcevisibility(0)
39555     open #(instr:=fngetfilenumber) : "SROW=3,SCOL=3,ROWS=13,COLS=24,Border=S,Caption=Change Control",display,outin 
39556     print #instr, fields mat centerspec$ : mat centertext$
39557 ! 
39558     do 
39559       if control then 
39560         let centertext$(_control)="Control: "&fncontroldescription$(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)(1:32)
39561       else 
39562         let centertext$(_control)="Control: None"
39563       end if 
39564       let centertext$(_type)="Type: "&fieldtype$
39565 ! 
39566       print #instr, fields mat centerspec$ : mat centertext$
39567 ! 
39568       input #0, fields "1,2,C 1,AEX" : key$
39569       let function=fkey
39570 ! 
39571       if function>=1400 and function<1500 then !  Process Button
39572         let fieldtype$=fnfieldtypecanswitch$(fnreadbutton$(function))
39573       end if 
39574 ! 
39575       if function>=1500 and function<=1500+udim(mat controlname$) then 
39576         if fncontrolcanswitch(function-1500,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
39577           let control=function-1500
39578         end if 
39579       end if 
39580 ! 
39581       if function=17 then 
39582 ! .         ! Switch the control type
39583         if fnswitchcontrol(control,fieldtype$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
39584           let changed=1
39585         end if 
39586       end if 
39587 ! 
39588     loop until function=99 or function=93 or function=44 or function=19 or changed or function>=1100 and function<1400
39589 ! 
39590     close #instr: 
39591   fnend 
39592 ! 
39593   def fnfieldtypecanswitch$(buttontext$)
39594     if lwrc$(buttontext$) = "field" or lwrc$(buttontext$) = "text box" then ! #Select# lwrc$(ButtonText$) #Case# "field" # "text box"
39595       let fnfieldtypecanswitch$="C"
39596     else if lwrc$(buttontext$) = "caption" then ! #Case# "caption"
39597       let fnfieldtypecanswitch$="caption"
39598     else if lwrc$(buttontext$) = "check box" then ! #Case# "check box"
39599       let fnfieldtypecanswitch$="check"
39600     else if lwrc$(buttontext$) = "combo box" then ! #Case# "combo box"
39601       let fnfieldtypecanswitch$="combo"
39602     else if lwrc$(buttontext$) = "button" then ! #Case# "button"
39603       let fnfieldtypecanswitch$="button"
39604     else if lwrc$(buttontext$) = "picture" then ! #Case# "picture"
39605       let fnfieldtypecanswitch$="P"
39606     end if  ! #End Select#
39607   fnend 
39608 ! 
39609   def fncontrolcanswitch(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
39610     if control and control<=udim(mat fieldtype$) then 
39611       if lwrc$(trim$(fieldtype$(control))) = "c" or lwrc$(trim$(fieldtype$(control))) = "caption" or lwrc$(trim$(fieldtype$(control))) = "check" or lwrc$(trim$(fieldtype$(control))) = "combo" or lwrc$(trim$(fieldtype$(control))) = "button" or lwrc$(trim$(fieldtype$(control))) = "p" then ! #Select# lwrc$(trim$(FieldType$(Control))) #Case# "c" # "caption" # "check" # "combo" # "button" # "p"
39612         let fncontrolcanswitch=1
39613       end if  ! #End Select#
39614     end if 
39615   fnend 
39616 ! 
39617   def fnsourcetype(fieldtype$)
39618     if lwrc$(trim$(fieldtype$)) = "c" or lwrc$(trim$(fieldtype$)) = "combo" then ! #Select# lwrc$(trim$(FieldType$)) #Case# "c" # "combo"
39619       let fnsourcetype=1
39620     else ! #Case Else#
39621       let fnsourcetype=0
39622     end if  ! #End Select#
39623   fnend 
39624 ! 
39625   def fnswitchcontrol(control,targettype$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,sourcetype,targettype)
39626     if control and len(trim$(targettype$)) then 
39627       if fncontrolcanswitch(control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
39628 ! .         ! Need to fill in the details of how this happens when control types
39629 ! .         !  aren't purely compatible
39630 ! 
39631 ! .         ! In Output Fields, Description is a hardcoded value; in Input fields, description is a link to another field.
39632         let sourcetype=fnsourcetype(fieldtype$(control))
39633         let targettype=fnsourcetype(targettype$)
39634 ! 
39635 ! .         ! If we change from one target type and another, have to erase the Description$ value
39636         if sourcetype><targettype then 
39637           let description$(control)=""
39638         end if 
39639 ! 
39640 ! .         ! We switch the actual control here
39641         let fieldtype$(control)=targettype$
39642         let fnswitchcontrol=1
39643       end if 
39644     end if 
39645   fnend 
39646 ! 
40000 !  #Autonumber# 40000,10
40010 ! ===================== Screen Functions ===================
40020 ! = These Variables Are Used Only In Full Screen Functions =
40030 ! ==========================================================
40040   dim static_screenvsize
40050   dim static_screenhsize
40060 ! 
40070 REDRAWENTIRESCREEN: !  Close And Reopen And Redraw The Entire Screen
40080   def fnredrawentirescreen(&wtoolbar,&wdebug,&weditor,mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
40090 ! 
40100     let fnclosetoolbarwindow
40110     let fnclosedebugwindow
40120     let fncloseeditorwindow
40130 ! 
40140     let fnopenmainwindows(mat screenio$, mat screenio)
40150     let fndisplaywindowsmenu
40160 ! 
40170     let fnpainttoolbarwindow(mat screenio$, mat screenio)
40180     let fnpaintdebugwindow
40190 ! 
40200   fnend 
40210 ! 
41000 !  #Autonumber# 41000,10
41010 OPENMAINWINDOWS: !  Open Main  Window,   Updating Size And Caption
41020   def fnopenmainwindows(mat screenio$, mat screenio;___, titlebar$*120,windowvsize, windowhsize, windowvposition, windowhposition, windowattributes$*140,toolbarwidth, toolbarheight, debugminheight, debugmaxheight, bordersize)
41030 ! 
41040     let debugminheight=7
41050     let debugmaxheight=10
41060 ! 
41070     if screenio(si_border) or pos(uprc$(screenio$(si_attributes)),"BORDER") then 
41080       let bordersize=2
41090     end if 
41100 ! 
41110     if trim$(screenio$(si_screencode))="" and trim$(screenio$(si_caption))="" then 
41120       let titlebar$="ScreenIO Program Generation System"
41130     else 
41140       let titlebar$="ScreenIO Program Generation System - Editing"
41150       if trim$(screenio$(si_screencode))<>"" then 
41160         let titlebar$=titlebar$ & "  " & trim$(screenio$(si_screencode))
41170       end if 
41180       if trim$(screenio$(si_caption))<>"" then 
41190         let titlebar$=titlebar$&' "'&screenio$(si_caption)&'"'
41200       end if 
41210     end if 
41220 ! 
41230     let toolbarwidth=26
41240     let static_screenvsize=max(screenio(si_vsize),24)+debugminheight+2+2+bordersize
41250     let static_screenhsize=max(screenio(si_hsize),80)+toolbarwidth+2+2+bordersize
41260     open #0: "ROWS=" & str$(static_screenvsize) & ", COLS="&str$(static_screenhsize)&", Caption="&titlebar$, display, outin 
41270 ! 
41280     let toolbarheight=static_screenvsize-2
41290     let fnopentoolbarwindow(toolbarheight,toolbarwidth)
41300 ! 
41310     let windowvposition=max(0,int((static_screenvsize-(debugmaxheight+2)-(screenio(si_vsize)+bordersize+2))/2))+2
41320     let windowhposition=max(0,int((static_screenhsize-(toolbarwidth+2)-(screenio(si_hsize)+bordersize))/2))+(toolbarwidth+2)+1
41330     let fnopeneditorwindow(windowvposition, windowhposition, mat screenio$, mat screenio)
41340 ! 
41350     let fnopendebugwindow(static_screenvsize,static_screenhsize,toolbarwidth,debugmaxheight,windowvposition+screenio(si_vsize)+bordersize+2)
41360 ! 
41370   fnend 
41380 ! 
41390 GETSCREENSIZE: ! Returns The Static Screen Size
41400   def fngetscreensize(;&vsize,&hsize)
41410     let vsize=static_screenvsize
41420     let hsize=static_screenhsize
41430   fnend 
41440 ! 
42000 !  #Autonumber# 42000,5
42005 !  ======================= Debug Window ======================
42010 !  = These Variables Are Used Only In Debug Window Functions =
42015 !  ===========================================================
42020   dim debugheadings$(3),debugwidths(3), debugspec$(3)
42025   dim debugobject(1) ,debugfield(1) ,debugmessage$(1)*255
42030   dim debugclearobject(1),debugclearfield(1),debugclearmessage$(1)*255
42035 ! 
42040   dim debug_start(1), debug_end(1), debug_color$(1)
42045 ! 
42050 OPENDEBUGWINDOW: !  Open Debug Window,  Set  "Static" Variables
42055   def fnopendebugwindow(screenvsize,screenhsize,toolbarwidth,debugmaxheight,topofspace;___,windowvposition,windowhposition)
42060 ! 
42065     let static_debugvsize=min(screenvsize-topofspace,debugmaxheight)
42070     let windowvposition=screenvsize-static_debugvsize
42075 ! 
42080     let windowhposition=toolbarwidth+4
42085     let static_debughsize=screenhsize-windowhposition
42090 ! 
42095     open #(static_wdebug:=fngetfilenumber): "SROW=" & str$(windowvposition) & ", SCOL=" & str$(windowhposition) & ",   ROWS=" & str$(static_debugvsize) & ",   COLS="& str$(static_debughsize)&",   Border=S,   Caption=Debug", display, outin 
42100 ! 
42105   fnend 
42110 ! 
42115 PAINTDEBUGWINDOW: !  Puts  The Listview In The Debug Window
42120   def fnpaintdebugwindow
42125 ! 
42130     let debugheadings$(1)="Object" ! Hidden Field
42135     let debugwidths(1)=0
42140     let debugspec$(1)="N 5"
42145     let debugheadings$(2)="Field" ! Hidden Field
42150     let debugwidths(2)=0
42155     let debugspec$(2)="N 5"
42160     let debugheadings$(3)="To Do:   "
42165     let debugwidths(3)=76
42170     let debugspec$(3)="CC 255"
42175 ! 
42180     let db_warning=1
42185     let db_error=2
42190 ! 
42195     let static_debuglistview$="1,1,LIST "&str$(static_debugvsize)&"/"&str$(static_debughsize)
42200 ! 
42205     print #static_wdebug, fields static_debuglistview$&",HEADERS,,1200" : (mat debugheadings$, mat debugwidths, mat debugspec$)
42210     let fncleardebuglistview
42215   fnend 
42220 ! 
42225 COLORDEBUGACTIVE: !  Colors  The Debug Window Yellow If Its Active
42230   def fncolordebugactive(;active,_,rowcount)
42235 ! 
42240     if active then 
42245       if static_debugactive then ! Do Nothing
42250       else 
42255         let static_debugactive=1
42260         input #static_wdebug, fields static_debuglistview$ & ",ROWCNT,ALL,NOWAIT" : rowcount
42265         if rowcount then 
42270 ! 
42275           let debug_start(1)=1
42280           let debug_end(1)=rowcount
42285           let debug_color$(1)="/#000000:#FFFF77"
42290           print #static_wdebug, fields static_debuglistview$ & ",ATTR" : (mat debug_start, mat debug_end, mat debug_color$)
42295         end if 
42300       end if 
42305     else 
42310       if static_debugactive then 
42315         let static_debugactive=0
42320         input #static_wdebug, fields static_debuglistview$ & ",ROWCNT,ALL,NOWAIT" : rowcount
42325         if rowcount then 
42330           let debug_start(1)=1
42335           let debug_end(1)=rowcount
42340           let debug_color$(1)="/#000000:#FFFFFF"
42345           print #static_wdebug, fields static_debuglistview$ & ",ATTR" : (mat debug_start, mat debug_end, mat debug_color$)
42350         end if 
42355       end if 
42360     end if 
42365   fnend 
42370 ! 
42375 CLEARDEBUGLISTVIEW: !   Clear The Debug List  View
42380   def fncleardebuglistview
42385     mat debugclearobject(0) : mat debugclearfield(0) : mat debugclearmessage$(0)
42390     print #static_wdebug, fields static_debuglistview$ &",=,1200" : (mat debugclearobject, mat debugclearfield, mat debugclearmessage$)
42395   fnend 
42400 ! 
42405 PRINTTODEBUGLISTVIEW: !  Prints Items To The Debug Listview
42410   def fnprinttodebuglistview(object,field,message$*255,errtype;___,populate$)
42415 ! 
42420     if clear then 
42425       let populate$=",="
42430     else 
42435       let populate$=",+"
42440     end if 
42445 ! 
42450     let debugobject(1)=object
42455     let debugfield(1)=field
42460     let debugmessage$(1)=message$
42465 ! 
42470     print #static_wdebug, fields static_debuglistview$ & populate$ & ",1200" : (mat debugobject, mat debugfield, mat debugmessage$)
42475     input #static_wdebug, fields static_debuglistview$ & ",ROWCNT,ALL,NOWAIT" : rowcount
42480 ! 
42485     let debug_start(1)=rowcount
42490     let debug_end(1)=rowcount
42495 ! 
42500     if errtype = db_warning then ! #Select# Errtype #Case# Db_Warning
42505       let debug_color$(1)="/#0033FF"
42510     else if errtype = db_error then ! #Case# Db_Error
42515       let debug_color$(1)="/#AA0000"
42520     else ! #Case Else#
42525       let debug_color$(1)="/#000000"
42530     end if  ! #End Select#
42535 ! 
42540     if static_debugactive then 
42545       let debug_color$(1)=debug_color$(1) & ":#FFFF77"
42550     else 
42555       let debug_color$(1)=debug_color$(1) & ":#FFFFFF"
42560     end if 
42565 ! 
42570     print #static_wdebug, fields static_debuglistview$ & ",ATTR,1200" : (mat debug_start, mat debug_end, mat debug_color$)
42575 ! 
42580   fnend 
42585 ! 
42590 READDEBUG: !  Reads The Debug Listview To Return The Last Thing The User Clicked On
42595   def fnreaddebug(&object;&field,&message$)
42600     input #static_wdebug, fields static_debuglistview$&",ROW,SEL,NOWAIT" : (mat debugobject, mat debugfield, mat debugmessage$)
42605 ! 
42610     let object=debugobject(1)
42615     let field=debugfield(1)
42620     let message$=debugmessage$(1) soflow ignore
42625 ! 
42630   fnend 
42635 ! 
42640 GETDEBUGSPEC: !  Return Debug Listview Spec For Input
42645   def fngetdebugspec(&spec$)
42650     let spec$ = static_debuglistview$&",ROWSUB,SELONE, 1200"
42655   fnend 
42660 ! 
42665 GETDEBUGWINDOW: ! Returns The Debug Window Number
42670   def fngetdebugwindow
42675     let fngetdebugwindow=static_wdebug
42680   fnend 
42685 ! 
42690 CLOSEDEBUGWINDOW: !  Close The Debug Window,  If Open
42695   def fnclosedebugwindow
42700     if static_wdebug and file(static_wdebug<>-1) then 
42705       close #static_wdebug: 
42710     end if 
42715     let static_wdebug=0
42720   fnend 
42725 ! 
43000 !  #Autonumber# 43000,10
43010 !  ======================= Toolbar Window ======================
43020 !  = These Variables Are Used Only In Toolbar Window Functions =
43030 !  =============================================================
43040   dim static_toolbarvsize, static_toolbarhsize
43050   dim static_wtoolbar
43060 ! 
43070   def fnopentoolbarwindow(toolbarvsize,toolbarhsize)
43080     let static_toolbarvsize=toolbarvsize
43090     let static_toolbarhsize=toolbarhsize
43100     open #(static_wtoolbar:=fngetfilenumber): "SROW=2,SCOL=2,ROWS="&str$(static_toolbarvsize)&",COLS="&str$(static_toolbarhsize)&", Border=S, Caption=Toolbar", display, outin 
43110   fnend 
43120 ! 
43130 PAINTTOOLBARWINDOW: !  Paint The Toolbar
43140   def fnpainttoolbarwindow(mat screenio$,mat screenio;___,numberofbuttons,buttonrows)
43150 ! 
43160     let numberofbuttons=fninitializebuttons
43170     let buttonrows=int((numberofbuttons+1)/2)
43180 ! 
43190     let fnopenattributeswindow(static_wtoolbar)
43200     let fnopenfieldswindow(static_wtoolbar,static_toolbarvsize,buttonrows+2)
43210     let fnopenbuttonwindow(static_wtoolbar,static_toolbarvsize,buttonrows)
43220 ! 
43230     let fnpaintattributeswindow(mat screenio$,mat screenio)
43240     let fnpaintfieldswindow
43250     let fnpaintbuttonwindow
43260 ! 
43270   fnend 
43280 ! 
43290 GETTOOLBARWINDOW: ! Return Toolbar Window Number
43300   def fngettoolbarwindow
43310     let fngettoolbarwindow=static_wtoolbar
43320   fnend 
43330 ! 
43340 CLOSETOOLBARWINDOW: !  Close The Toolbar Window,  If Open
43350   def fnclosetoolbarwindow
43360 ! 
43370     let fncloseattributeswindow
43380     let fnclosefieldswindow
43390     let fnclosebuttonwindow
43400 ! 
43410     if static_wtoolbar and file(static_wtoolbar<>-1) then 
43420       close #static_wtoolbar: 
43430     end if 
43440     let static_wtoolbar=0
43450   fnend 
43460 ! 
44000 !  #Autonumber# 44000,10
44010 ! ====================== Attributes Window =======================
44020 ! = These Variables Are Used Only In Attributes Window Functions =
44030 ! ================================================================
44040 ! 
44050   dim wattributes
44060   dim attributecaptions$(1)
44070   dim attributecapspec$(1)
44080   dim attributespec$(1)*60
44090   dim attributedata$(1)*255
44100   dim attributettip$(1)*255
44110   dim attributesubs(1)
44120 ! 
44130 OPENATTRIBUTESWINDOW: !  Open  The Attributes Window And Set The Statics
44140   def fnopenattributeswindow(wtoolbar)
44150     open #(wattributes:=fngetfilenumber) : "SROW=2,SCOL=2,ROWS=13,COLS=24,Border=S,Caption=Window Attributes,Parent="&str$(wtoolbar),display,outin 
44160   fnend 
44170 ! 
44180 PAINTATTRIBUTESWINDOW: !  Paint  The Attributes Window
44190   def fnpaintattributeswindow(mat screenio$, mat screenio;___,i)
44200     let fndefineattributeswindow(mat attributecaptions$, mat attributecapspec$,mat attributespec$,mat attributedata$,mat attributettip$,mat attributesubs)
44210 ! 
44220     let fnfiletoscreen(mat screenio$,mat screenio, mat attributedata$, mat attributesubs)
44230     print #wattributes, fields mat attributecapspec$ : mat attributecaptions$
44240     print #wattributes, fields mat attributespec$, help mat attributettip$ : mat attributedata$
44250   fnend 
44260 ! 
44270 GETATTRIBUTESSPEC: !  Return  The Input Arrays
44280   def fngetattributespec(mat spec$, mat data$, mat ttip$, mat subs;___,index)
44290     let fndefineattributeswindow(mat attributecaptions$, mat attributecapspec$,mat attributespec$,mat attributedata$,mat attributettip$,mat attributesubs)
44300 ! 
44310     mat spec$(udim(mat attributespec$))=attributespec$
44320     mat data$(udim(mat attributedata$))=attributedata$
44330     mat subs(udim(mat attributesubs))=attributesubs
44340     mat ttip$(udim(mat attributettip$))=attributettip$
44350 ! 
44360     for index=1 to udim(mat spec$)
44370       let spec$(index)=srep$(uprc$(spec$(index)),",S,",",/#000000:#FFFF77,")
44380     next index
44390 ! 
44400   fnend 
44410 ! 
44420 SETINPUTATTRIBUTESCONTROL: ! Set The Attributes Control Based On The Fkey Passed In
44430   def fnsetinputattributescontrol(function)
44440     let fndefineattributeswindow(mat attributecaptions$, mat attributecapspec$,mat attributespec$,mat attributedata$,mat attributettip$,mat attributesubs)
44450     let fnsetinputattributescontrol=attributesubs(function-1100)
44460   fnend 
44470 ! 
44480 COLORATTRIBUTEACTIVE: ! Return The Input Arrays
44490   def fncolorattributeactive(active,mat screenio$,mat screenio;___,index)
44500 ! 
44510     let fndefineattributeswindow(mat attributecaptions$, mat attributecapspec$,mat attributespec$,mat attributedata$,mat attributettip$,mat attributesubs)
44520     let fnfiletoscreen(mat screenio$,mat screenio, mat attributedata$,mat attributesubs)
44530     let fnadjustcolorbuttons(mat attributespec$,mat attributedata$,mat attributesubs)
44540 ! .    !
44550     if active then 
44560       for index=1 to udim(mat attributespec$)
44570         let attributespec$(index)=srep$(uprc$(attributespec$(index)),",S,",",S/#000000:#FFFF77,")
44580       next index
44590     else 
44600       for index=1 to udim(mat attributespec$)
44610         let attributespec$(index)=srep$(uprc$(attributespec$(index)),",S,",",S/#000000:#FFFFFF,")
44620       next index
44630     end if 
44640 ! 
44650     print #wattributes, fields mat attributespec$, help mat attributettip$ : mat attributedata$
44660 ! 
44670   fnend 
44680 ! 
44690 ! 
44700 GETATTRIBUTESWINDOW: ! Return Attributes Window Number
44710   def fngetattributeswindow
44720     let fngetattributeswindow = wattributes
44730   fnend 
44740 ! 
44750 CLOSEATTRIBUTESWINDOW: !  Close The Attributes Window,   If Open
44760   def fncloseattributeswindow
44770     if wattributes and file(wattributes<>-1) then 
44780       close #wattributes: 
44790     end if 
44800     let wattributes=0
44810   fnend 
44820 ! 
45000 !   #Autonumber# 45000,5
45005 !  ====================== Fields Window =======================
45010 !  = These Variables Are Used Only In Fields Window Functions =
45015 !  ============================================================
45020 ! 
45025   dim fieldsheadings$(3),fieldswidths(3), fieldsspec$(3)
45030   dim fieldsname$(1)*30,fieldsdescription$(1)*60,fieldstype$(1)
45035   dim fields_start(1),fields_end(1),fields_color$(1)
45040 ! 
45045   dim static_fieldslistview$*80
45050   dim static_fieldscount
45055   dim static_fieldsvsize, static_fieldshsize
45060   dim static_wfields
45065 ! 
45070 OPENFIELDSWINDOW: !  Open  The Fields Window Here
45075   def fnopenfieldswindow(wtoolbar,toolbarvsize,buttonsize)
45080     let static_fieldsvsize=toolbarvsize - buttonsize - 17
45085     let static_fieldshsize=24
45090     open #(static_wfields:=fngetfilenumber) : "SROW=17,SCOL=2,ROWS="&str$(static_fieldsvsize)&",COLS="&str$(static_fieldshsize)&",Border=S, Caption=Field List,Parent="&str$(wtoolbar),display,outin 
45095   fnend 
45100 ! 
45105 PAINTFIELDSWINDOW: !  Put The Listview In The Fields Window Here
45110   def fnpaintfieldswindow
45115 ! 
45120     let fieldsheadings$(1)="Name"
45125     let fieldswidths(1)=9
45130     let fieldsspec$(1)="V 50"
45135     let fieldsheadings$(2)="Description"
45140     let fieldswidths(2)=14
45145     let fieldsspec$(2)="V 255"
45150     let fieldsheadings$(3)="Type"
45155     let fieldswidths(3)=5
45160     let fieldsspec$(3)="V 30"
45165 ! 
45170     let static_fieldslistview$="1,1,LIST "&str$(static_fieldsvsize)&"/"&str$(static_fieldshsize)
45175 ! 
45180     print #static_wfields, fields static_fieldslistview$&",HEADERS,,1300" : (mat fieldsheadings$, mat fieldswidths, mat fieldsspec$)
45185   fnend 
45190 ! 
45195 COLORFIELDSACTIVE: !  Colors The Debug Window Yellow If Its Active
45200   def fncolorfieldsactive(;active,_,rowcount)
45205     if active then 
45210       if static_fieldsactive then ! Do Nothing
45215       else 
45220         let static_fieldsactive=1
45225         input #static_wfields, fields static_fieldslistview$ & ",ROWCNT,ALL,NOWAIT" : rowcount
45230         if rowcount then 
45235           let fields_start(1)=1
45240           let fields_end(1)=rowcount
45245           let fields_color$(1)="/#000000:#FFFF77"
45250           print #static_wfields, fields static_fieldslistview$ & ",ATTR,1300" : (mat fields_start, mat fields_end, mat fields_color$)
45255         end if 
45260       end if 
45265     else 
45270       if static_fieldsactive then 
45275         let static_fieldsactive=0
45280         input #static_wfields, fields static_fieldslistview$ & ",ROWCNT,ALL,NOWAIT" : rowcount
45285         if rowcount then 
45290           let fields_start(1)=1 !:
                let fields_end(1)=rowcount
45295           let fields_color$(1)="/#000000:#FFFFFF"
45300           print #static_wfields, fields static_fieldslistview$ & ",ATTR,1300" : (mat fields_start, mat fields_end, mat fields_color$)
45305         end if 
45310       else ! Do Nothing
45315       end if 
45320     end if 
45325   fnend 
45330 ! 
45335 REPOPULATEFIELDSLISTVIEW: !  Prints  Items  To  The Debug Listview
45340   def fnrepopulatefieldslistview(layoutname$;&prefix$)
45345 ! 
45350     if trim$(layoutname$)<>"" and fndoeslayoutexist(screenio$(si_filelay)) then 
45355       let fnreadlayoutarrays(layoutname$,prefix$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldssspec$,mat fieldsnspec$,mat fieldssdescription$,mat fieldsndescription$)
45360 ! 
45365       print #static_wfields, fields static_fieldslistview$ & ",=,1300" : (mat fieldsssubs$, mat fieldssdescription$, mat fieldssspec$)
45370       print #static_wfields, fields static_fieldslistview$ & ",+,1300" : (mat fieldsnsubs$, mat fieldsndescription$, mat fieldsnspec$)
45375       let static_fieldscount=udim(mat fieldsssubs$) + udim(mat fieldsnsubs$)
45380 ! 
45385     else 
45390       mat fieldsssubs$(0) : mat fieldssspec$(0) : mat fieldssdescription$(0)
45395       print #static_wfields, fields static_fieldslistview$ & ",=,1300" : (mat fieldsssubs$, mat fieldssspec$, mat fieldssdescription$)
45400       let static_fieldscount=0
45405     end if 
45410   fnend 
45415 ! 
45420 READFIELDS: ! Reads The Debug Listview To Return The Last Thing The User Clicked On
45425   def fnreadfields(&name$,&description$,&type$)
45430     mat fieldsname$(1) : mat fieldsdescription$(1) : mat fieldstype$(1)
45435 ! 
45440     input #static_wfields, fields static_fieldslistview$&",ROW,SEL,NOWAIT" : (mat fieldsname$, mat fieldsdescription$, mat fieldstype$)
45445     if udim(mat fieldsname$) and udim(mat fieldsdescription$) and udim(mat fieldstype$) then 
45450       let name$=trim$(fieldsname$(1))(1:50)
45455       let description$=trim$(fieldsdescription$(1))(1:255)
45460       let type$=trim$(fieldstype$(1))(1:30)
45465 ! 
45470       let fnreadfields=1
45475     end if 
45480   fnend 
45485 ! 
45490 GETFIELDSSPEC: !  Returns  The Fields Listview Spec For Input
45495   def fngetfieldsspec(&spec$)
45500     let spec$=static_fieldslistview$&",ROWSUB,SELONE,1300"
45505   fnend 
45510 ! 
45515 GETFIELDSCOUNT: ! Returns The Fields Count
45520   def fngetfieldscount
45525     let fngetfieldscount=static_fieldscount
45530   fnend 
45535 ! 
45540 GETFIELDSWINDOW: ! Return  The Fields Window Number
45545   def fngetfieldswindow
45550     let fngetfieldswindow=static_wfields
45555   fnend 
45560 ! 
45565 CLOSEFIELDSWINDOW: !   Close  The Debug Window,   If Open
45570   def fnclosefieldswindow
45575     if static_wfields and file(static_wfields<>-1) then 
45580       close #static_wfields: 
45585     end if 
45590     let static_wfields=0
45595   fnend 
45600 ! 
46000 ! #Autonumber# 46000,5
46005 ! ==================== Buttons Window =========================
46010 ! = These Variables Are Used Only In Buttons Window Functions =
46015 ! =============================================================
46020 ! 
46025   dim static_wbutton
46030   dim static_buttonvsize
46035   dim buttonspec$(1)*30
46040   dim static_buttonfkey
46045   dim buttons$(1)*30, buttontooltip$(1)*255
46050 ! 
46055 INITIALIZEBUTTONS: !   Initialize The Buttons Static Variables
46060   def fninitializebuttons(;___,index)
46065     let fndefinebuttons(mat buttons$, mat buttontooltip$)
46070     let fninitializebuttons=udim(mat buttons$)
46075   fnend 
46080 ! 
46085   dim validdelim$*64
46090   def fnfixtooltips(mat x$;___,index,ttipver,delim$,itsvalid,jndex)
46095     if fn42 then let ttipver=4 else let ttipver=3
46100     let validdelim$=";:|\`~!@#$%^&*()-_=+{[}]'"",<.>/?"
46105     for index=1 to udim(mat x$)
46110 ! 
46115       if len(trim$(x$(index))) then 
46120         let itsvalid=0
46125 ! 
46130         if fnisdigit(x$(index)(1:1)) then 
46135           let delim$=x$(index)(len(x$(index)):len(x$(index)))
46140           if delim$=x$(index)(2:2) then 
46145             if ~pos(x$(index)(3:len(x$(index))-1),delim$) then 
46150               let itsvalid=1
46155             end if 
46160           else if x$(index)(2:2)=";" then 
46165             if ~pos(x$(index)(3:len(x$(index))),";") then 
46170               let x$(index)=x$(index)&";"
46175               let itsvalid=1
46180             end if 
46185           end if 
46190         end if 
46195 ! 
46200         if ~itsvalid then 
46205           let jndex=0
46210           do while jndex<len(validdelim$)
46215             let jndex+=1
46220           loop while pos(x$(index),validdelim$(jndex:jndex))
46225           if pos(x$(index),validdelim$(jndex:jndex)) then 
46230             let x$(index)=srep$(x$(index),";",":")
46235             let jndex=pos(validdelim$,";")
46240           end if 
46245 ! 
46250           let x$(index)=str$(ttipver)&validdelim$(jndex:jndex)&x$(index)&validdelim$(jndex:jndex)
46255 ! 
46260         end if 
46265       end if 
46270     next index
46275   fnend 
46280 ! 
46285   def fnisdigit(x$;___,index,notnumber)
46290     for index=1 to len(x$)
46295       if ~pos("0123456789",x$(index:index)) then 
46300         let notnumber=1
46305       end if 
46310     next index
46315     let fnisdigit=~notnumber
46320   fnend 
46325 ! 
46330 OPENBUTTONWINDOW: !  Open  The Buttons Window And Set  The Statics
46335   def fnopenbuttonwindow(wtoolbar,toolbarvsize,buttonsize;___,buttonhposition,buttonvposition,buttonhsize,buttonvsize)
46340     open #(static_wbutton:=fngetfilenumber) : "SROW="&str$(toolbarvsize-buttonsize)&",SCOL=2,ROWS="&str$(buttonsize)&", COLS=24, Border=S, Caption=Toolbox, Parent="&str$(wtoolbar),display,outin 
46345     let static_buttonvsize=buttonsize
46350     let static_buttonfkey=1400
46355   fnend 
46360 ! 
46365 PAINTBUTTONWINDOW: !  Paint  The Buttons On  The Window
46370   def fnpaintbuttonwindow(;___,index)
46375     mat buttonspec$(udim(mat buttons$))
46380     for index=1 to udim(mat buttons$)
46385       let buttonspec$(index)=str$(int((index-1)/2)+1)&","&str$(2+(11*mod((index-1),2)))&","&str$(11-mod((index),2))&"/CC 22,/W:W,B"&str$(static_buttonfkey+index)
46390     next index
46395     print #static_wbutton, fields mat buttonspec$, help mat buttontooltip$ : mat buttons$
46400   fnend 
46405 ! 
46410 READBUTTON: !  Read Which Button  Was  Pressed
46415   def fnreadbutton$*30(;fkeyvalue,_,index)
46420     if fkeyvalue=0 then let fkeyvalue=fkey
46425 ! 
46430     if fkeyvalue>static_buttonfkey and fkeyvalue<=static_buttonfkey+udim(buttons$) then 
46435       let fnreadbutton$=buttons$(fkeyvalue-static_buttonfkey)
46440     end if 
46445   fnend 
46450 ! 
46455 GETBUTTONWINDOW: ! Return Attributes Window Number
46460   def fngetbuttonwindow
46465     let fngetbuttonwindow = static_wbutton
46470   fnend 
46475 ! 
46480 CLOSEBUTTONWINDOW: !   Close  The Button  Window,   If Open
46485   def fnclosebuttonwindow
46490     if static_wbutton and file(static_wbutton<>-1) then 
46495       close #static_wbutton: 
46500     end if 
46505     let static_wbutton=0
46510   fnend 
46515 ! 
47000 !   #Autonumber# 47000,10
47010 !  ====================== Editor Window =======================
47020 !  = These Variables Are Used Only In Editor Window Functions =
47030 !  ============================================================
47040 ! 
47050   dim static_weditor,static_editorvpos,static_editorhpos
47060   dim static_wcontainer,static_containervpos,static_containerhpos
47070 ! 
47080 OPENEDITORWINDOW: !  Open The Editor Window
47090   def fnopeneditorwindow(windowvposition, windowhposition, mat screenio$, mat screenio;___,bordersize,caption$*80,hpos,vpos)
47100 ! 
47110     let static_editorhpos=1
47120     let static_editorvpos=1
47130     if screenio(si_border) or pos(uprc$(screenio$(si_attributes)),"BORDER") then 
47140       let bordersize=2
47150       let static_editorhpos=2
47160       let static_editorvpos=2
47170     end if 
47180 ! 
47190     if trim$(screenio$(si_caption))<>"" then 
47200       let caption$=",CAPTION=Editor -  "&trim$(screenio$(si_caption))
47210     else 
47220       let caption$=",CAPTION=Editor"
47230     end if 
47240 ! 
47250     open #(static_wcontainer:=fngetfilenumber): "SROW="&str$(windowvposition)&",SCOL="&str$(windowhposition)&",ROWS="&str$(screenio(si_vsize)+bordersize)&",COLS="&str$(screenio(si_hsize)+bordersize)&",BORDER=S"&caption$,display,outin 
47260 ! 
47270     let static_containervpos=windowvposition
47280     let static_containerhpos=windowhposition
47290 ! 
47300     let static_weditor=fnopenwindow(static_editorvpos,static_editorhpos,mat screenio$, mat screenio,static_wcontainer)
47310 ! 
47320   fnend 
47330 ! 
47340 GETEDITORWINDOW: !  Return  Window Number For Main Editor Window
47350   def fngeteditorwindow
47360     let fngeteditorwindow=static_weditor
47370   fnend 
47380 ! 
47390 GETEDITORPOSITION: !  Return Window Position For Main Editor Window
47400   def fngeteditorposition(;&vpos,&hpos)
47410     let vpos=static_editorvpos+static_containervpos-1
47420     let hpos=static_editorhpos+static_containerhpos-1
47430   fnend 
47440 ! 
47450 CLOSEEDITORWINDOW: !  Close Editor Window If Its Open
47460   def fncloseeditorwindow
47470     if static_weditor then 
47480       if file(static_weditor)<>-1 then 
47490         close #static_weditor: 
47500       end if 
47510       let static_weditor=0
47520       let static_editorhpos=0
47530       let static_editorvpos=0
47540     end if 
47550 ! 
47560     if static_wcontainer then 
47570       if file(static_wcontainer)<>-1 then 
47580         close #static_wcontainer: 
47590       end if 
47600 ! 
47610       let static_wcontainer=0
47620       let static_containervpos=0
47630       let static_containerhpos=0
47640 ! 
47650     end if 
47660 ! 
47670   fnend 
47680 ! 
48000 !  #Autonumber# 48000,2
48002 !  ======================================================================
48004 !  ========================== Define Functions ==========================
48006 !  ======================================================================
48008 !  These Functions Define The Way The Windows Menu,  Buttons,  Etc Look
48010 !  And The Actions Associated With Them
48012 ! 
48014 DEFINEWINDOWSMENU: !  Define The Windows Menu
48016   def fndefinewindowsmenu(mat m$, mat pgm$, mat status$;___,index)
48018 ! 
48020     restore DEFINEWINDOWSMENU
48022     data "&File","",""
48024     data "  &New","new","E"
48026     data "  &Load","load","E"
48028     data "  &Save and Compile","save","E"
48030     data "  &Compile","recompileone","E"
48032     data "  Save and &Test","saveandtest","E"
48034     data "  -","",""
48036     data "  E&xport Screen","export","E"
48038     data "  &Import Screen","import","E"
48040     data "  -","",""
48042     data "  &Purge ScreenFlds File","purge","E"
48044     data "  Recompile &All Screens","recompile","E"
48046     data "  -","",""
48048     data "  &FileIO","fileio","E"
48050     data "  New &Window","newwindow","E"
48052     data "  &BR Console","brconsole","E"
48054     data "  &Explore Local Folder","windowsexplorer","E"
48056     data "  -","",""
48058     data "  &Quit","quit","E"
48060     data "&Options","",""
48062     data "  Click To &Move","clickmove","ECX"
48064     data "  Preview &Listviews","poplist","ECX"
48066     data "  Real-Time &Filters","filter","EC"
48068     data "&Tools","",""
48070     data "  Power &Search","search","E"
48072     data "  &Code Explorer","codeexplore","E"
48074     data "  -","",""
48076     data "  &Generate Screen","generatescreen","E"
48078     data "  &Generate Code","generatecode","E"
48080     data "  -","",""
48082     data "  &Orphaned Functions","orphanedfunctions","E"
48084     data "&Control","",""
48086     data "  Add &Field","field","E"
48088     data "  Add E&xit Buttons","exit buttons","E"
48090     data "  Add &Add/Edit Buttons","ae buttons","E"
48092     data "  Add Add/Edit/&Delete Buttons","aed buttons","E"
48094     data "  -","",""
48096     data "  Add &Text Box","text box","E"
48098     data "  Add &Caption","caption","E"
48100     data "  Add C&heck Box","check box","E"
48102     data "  Add C&ombo Box","combo box","E"
48104     data "  Add &Listview","listview","E"
48106     data "  Add &Search Box","search box","E"
48108     data "  Add F&ilter Box","filter box","E"
48110     data "  Add &Button","button","E"
48112     data "  Add &Picture","picture","E"
48114 ! .   ! data "  Add C&alendar Button","calendar","E"
48116     data "  Add &Frame","frame","E"
48118     data "  Add Sc&reen","screen","E"
48120     data "  -","",""
48122     data "  S&kip a Space","skip a space","E"
48124     data "  -","",""
48126     data "  Change Control T&ype","changetype","E"
48128     data "&Screen","",""
48130     data "  &Adjust Screen Size","resize screen","E"
48132     data "  &Move Controls","movemode","E"
48134     data "  &Draw Movement Grid","drawdots","E"
48136     data "  Visit &Checklist","debuglist","E"
48138     data "  -","",""
48140     data "  Set &FG Color","setfgcolor","E"
48142     data "  Set &BG Color","setbgcolor","E"
48144     data "  Select File &Layout","selectlayout","E"
48146     data "  Set &Events","events","E"
48148     data "  Set Tab &Order","taborder","E"
48150     data "  -","",""
48152     data "  Configure &Debug","debug","E"
48154     data "  Configure Additional &Info","protected","E"
48156     data "  -","",""
48158     data "  &Test Screen","test","E"
48160     data "&Help","",""
48162     data "  &Documentation","help","E"
48164     data "  -","",""
48166     data "  &About","about","E"
48168 ! 
48170     mat m$(0) : mat pgm$(0) : mat status$(0)
48172 READNEXTMENU: ! Read The Next Menu Option Here
48174     mat m$(udim(mat m$)+1) : mat pgm$(udim(mat m$)) : mat status$(udim(mat m$))
48176     read m$(udim(mat m$)), pgm$(udim(mat m$)), status$(udim(mat m$)) eof DONEREADMENU
48178     goto READNEXTMENU
48180 DONEREADMENU: !  Finished Reading The Menu
48182 ! 
48184     let fnsettings
48186 ! 
48188 ! .   ! Apply Checkmark Environment Variables
48190     for index=1 to udim(mat pgm$)
48192       if pos(uprc$(status$(index)),"C") then 
48194         if pos(uprc$(trim$(env$(pgm$(index)))),"Y") then 
48196           if ~(pos(uprc$(status$(index)),"X")) then 
48198             let status$(index)=status$(index)&"X"
48200           end if 
48202         else if pos(uprc$(trim$(env$(pgm$(index)))),"N") then 
48204           let status$(index)=srep$(uprc$(status$(index)),"X","")
48206         else 
48208 ! .            ! No env dollar, use ini file, then update setenv
48210           if lwrc$(trim$(pgm$(index))) = "clickmove" then ! #Select# lwrc$(trim$(Pgm$(Index))) #Case# "clickmove"
48212             if setting_clicktomove then 
48214               if ~(pos(uprc$(status$(index)),"X")) then 
48216                 let status$(index)=status$(index)&"X"
48218               end if 
48220             else 
48222               let status$(index)=srep$(uprc$(status$(index)),"X","")
48224             end if 
48226           else if lwrc$(trim$(pgm$(index))) = "poplist" then ! #Case# "poplist"
48228             if setting_previewlistviews then 
48230               if ~(pos(uprc$(status$(index)),"X")) then 
48232                 let status$(index)=status$(index)&"X"
48234               end if 
48236             else 
48238               let status$(index)=srep$(uprc$(status$(index)),"X","")
48240             end if 
48242           else if lwrc$(trim$(pgm$(index))) = "filter" then ! #Case# "filter"
48244             if setting_realtimefilters then 
48246               if ~(pos(uprc$(status$(index)),"X")) then 
48248                 let status$(index)=status$(index)&"X"
48250               end if 
48252             else 
48254               let status$(index)=srep$(uprc$(status$(index)),"X","")
48256             end if 
48258           end if  ! #End Select#
48260 ! 
48262           if pos(uprc$(status$(index)),"X") then 
48264             let setenv(pgm$(index),"Y")
48266           else 
48268             let setenv(pgm$(index),"N")
48270           end if 
48272         end if 
48274       end if 
48276     next index
48278 ! 
48280   fnend 
48282 ! 
48284 PROCESSWINDOWSMENU: !   Process A  Command From  The Windows Menu
48286   def fnprocesswindowsmenu(mat screenio$, mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,screen$)
48288 ! 
48290     dim menunew, menuload, menusave, menuquit, menuabout, menuhelp, menuevents, menutaborder
48292     dim menuexport, menuimport, menurecompile, menutest, menurecompileone, menupurge, menudebug
48294     dim menufgcolor, menubgcolor, menuselectlayout, menumovement, menudebuglist, menufieldslist
48296     dim menuaddcontrol, menuprotected
48298 ! 
48300     let menunew=1 : let menuload=2 : let menusave=3 : let menuquit=4
48302     let menuabout=5 : let menuhelp=6 : let menuevents=7 : let menutaborder=8
48304     let menuexport=9 : let menuimport=10 : let menurecompile=11 : let menutest=12
48306     let menurecompileone=13 : let menupurge=14 : let menudebug=15
48308     let menufgcolor=16 : let menubgcolor=17 : let menuselectlayout=18 : let menumovement=19
48310     let menudebuglist=20 : let menufieldslist=21 : let menuaddcontrol=22 : let menuprotected=23
48312 ! 
48314     if lwrc$(menu$) = "new" then ! #Select# Lwrc$(Menu$) #Case# "new"
48316       if fnoktoproceed(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48318         let fnreadscreen("",mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48320         let fnprocesswindowsmenu=menunew
48322       end if 
48324 ! 
48326     else if lwrc$(menu$) = "load" then ! #Case# "load"
48328       if fnoktoproceed(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48330         let screen$=fnselectscreen$
48332         if trim$(screen$)<>"" then 
48334           if fnreadscreen(screen$,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48336             let fnprocesswindowsmenu=menuload
48338           else 
48340             let msgbox("Error Reading Screen - Screen not  found","Error","OK","ERR")
48342           end if 
48344         end if 
48346       end if 
48348 ! 
48350     else if lwrc$(menu$) = "recompileone" then ! #Case# "recompileone"
48352       if fncompilehelperlibrary(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48354         let fnprocesswindowsmenu=menurecompileone
48356       end if 
48358 ! 
48360     else if lwrc$(menu$) = "save" then ! #Case# "save"
48362       if fnwritescreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48364         let fnprocesswindowsmenu=menusave
48366       end if 
48368 ! 
48370     else if lwrc$(menu$) = "saveandtest" then ! #Case# "saveandtest"
48372       if fnwritescreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1) then 
48374         execute "system -C -M "&fngetbrexe$&" -"&fngetwbcfg$&" run "&setting_screenfolder$&"\"&trim$(screenio$(si_screencode))
48376         let fnprocesswindowsmenu=menutest
48378       end if 
48380 ! 
48382     else if lwrc$(menu$) = "quit" then ! #Case# "quit"
48384       if fnoktoproceed(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48386         let fnprocesswindowsmenu=menuquit
48388       end if 
48390 ! 
48392     else if lwrc$(menu$) = "brconsole" then ! #Case# "brconsole"
48394       execute "system -C -M "&fngetbrexe$&" -"&fngetwbcfg$
48396 ! 
48398     else if lwrc$(menu$) = "windowsexplorer" then ! #Case# "windowsexplorer"
48400       execute "system -C start "&os_filename$(".")
48402 ! 
48404     else if lwrc$(menu$) = "newwindow" then ! #Case# "newwindow"
48406       execute "system -C -M "&fngetbrexe$&" -"&fngetwbcfg$&" run "&setting_screeniopath$
48408 ! 
48410     else if lwrc$(menu$) = "fileio" then ! #Case# "fileio"
48412       execute "system -C -M "&fngetbrexe$&" -"&fngetwbcfg$&" run "&setting_fileiopath$
48414 ! 
48416     else if lwrc$(menu$) = "export" then ! #Case# "export"
48418       if fnexportscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48420         let fnprocesswindowsmenu=menuexport
48422       end if 
48424 ! 
48426     else if lwrc$(menu$) = "import" then ! #Case# "import"
48428       if fnoktoproceed(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48430         if fnimportscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48432           let fnprocesswindowsmenu=menuimport
48434         end if 
48436       end if 
48438 ! 
48440     else if lwrc$(menu$) = "recompile" then ! #Case# "recompile"
48442       if fnoktoproceed(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48444         let fnrecompileallscreens(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48446         let fnprocesswindowsmenu=menurecompile
48448       end if 
48450 ! 
48452     else if lwrc$(menu$) = "purge" then ! #Case# "purge"
48454       if fnoktoproceed(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48456         let fnclosescreenfiles
48458         if exists("screen\temp.dat") then 
48460           print "There is already temp data present. Please Delete it and retry."
48462           pause  ! Give Programmer Chance To Recover.
48464         end if 
48466         if exists("screen\oldscreenfld.dat") then execute "free screen\oldscreenfld.dat"
48468         if exists("screen\oldscreenfld.key") then execute "free screen\oldscreenfld.key"
48470         if exists("screen\oldscreenfld.ky2") then execute "free screen\oldscreenfld.ky2"
48472         execute "copy screen\screenfld.dat screen\temp.dat -d"
48474         execute "rename screen\screenfld.dat screen\oldscreenfld.dat"
48476         execute "rename screen\temp.dat screen\screenfld.dat"
48478         execute "rename screen\screenfld.key screen\oldscreenfld.key"
48480         execute "rename screen\screenfld.ky2 screen\oldscreenfld.ky2"
48482         let fnupdatefile("screenfld")
48484         let fnopenscreenfiles(mat screenio$,mat screenio)
48486         let fnprocesswindowsmenu=menupurge
48488       end if 
48490 ! 
48492     else if lwrc$(menu$) = "test" then ! #Case# "test"
48494       if len(trim$(screenio$(si_screencode))) and exists(setting_screenfolder$&"\"&trim$(screenio$(si_screencode))&".br") then 
48496         execute "system -C -M "&fngetbrexe$&" -"&fngetwbcfg$&" run "&setting_screenfolder$&"\"&trim$(screenio$(si_screencode))
48498         let fnprocesswindowsmenu=menutest
48500       else 
48502         let msgbox("You must save your screen before testing it.")
48504       end if 
48506     else if lwrc$(menu$) = "help" then ! #Case# "help"
48508       execute "system -C -M start http://www.brwiki.com/index.php?title=ScreenIO_Library"
48510       let fnprocesswindowsmenu=menuhelp
48512     else if lwrc$(menu$) = "about" then ! #Case# "about"
48514       let fnshowabout
48516       let fnprocesswindowsmenu=menuabout
48518     else if lwrc$(menu$) = "events" then ! #Case# "events"
48520       let fnprocesswindowsmenu=menuevents
48522     else if lwrc$(menu$) = "taborder" then ! #Case# "taborder"
48524       let fnprocesswindowsmenu=menutaborder
48526     else if lwrc$(menu$) = "debug" then ! #Case# "debug"
48528       let fnprocesswindowsmenu=menudebug
48530     else if lwrc$(menu$) = "protected" then ! #Case# "protected"
48532       let fnprocesswindowsmenu=menuprotected
48534     else if lwrc$(menu$) = "field" then ! #Case# "field"
48536       let fnprocesswindowsmenu=menufieldslist
48538     else if lwrc$(menu$) = "text box" or lwrc$(menu$) = "caption" or lwrc$(menu$) = "check box" or lwrc$(menu$) = "listview" or lwrc$(menu$) = "search box" or lwrc$(menu$) = "button" or lwrc$(menu$) = "picture" or lwrc$(menu$) = "skip a space" or lwrc$(menu$) = "frame" or lwrc$(menu$) = "screen" or lwrc$(menu$) = "combo box" or lwrc$(menu$) = "filter box" or lwrc$(menu$) = "calendar" then ! #Case# "text box" # "caption" # "check box" # "listview" # "search box" # "button" # "picture" # "skip a space" # "frame" # "screen" # "combo box" # "filter box" # "calendar"
48540       let fnprocesswindowsmenu=menuaddcontrol
48542     else if lwrc$(menu$) = "movemode" then ! #Case# "movemode"
48544       let fnprocesswindowsmenu=menumovement
48546     else if lwrc$(menu$) = "debuglist" then ! #Case# "debuglist"
48548       let fnprocesswindowsmenu=menudebuglist
48550     else if lwrc$(menu$) = "setfgcolor" then ! #Case# "setfgcolor"
48552       let fnprocesswindowsmenu=menufgcolor
48554     else if lwrc$(menu$) = "setbgcolor" then ! #Case# "setbgcolor"
48556       let fnprocesswindowsmenu=menubgcolor
48558     else if lwrc$(menu$) = "selectlayout" then ! #Case# "selectlayout"
48560       let fnprocesswindowsmenu=menuselectlayout
48562     else if lwrc$(menu$) = "changetype" then ! #Case# "changetype"
48564       let fnchangecontroltype(mat screenio$,mat screenio,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48566     else if lwrc$(menu$) = "resize screen" then ! #Case# "resize screen"
48568       let fnresizescreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48570     else if lwrc$(menu$) = "exit buttons" then ! #Case# "exit buttons"
48572       let fnaddexitbuttons(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48574     else if lwrc$(menu$) = "ae buttons" then ! #Case# "ae buttons"
48576       let fnaddaebuttons(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,"",0,0,0,0,1) ! Suppress Delete Button
48578     else if lwrc$(menu$) = "aed buttons" then ! #Case# "aed buttons"
48580       let fnaddaebuttons(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48582     else if lwrc$(menu$) = "search" then ! #Case# "search"
48584       let fnpowersearch
48586     else if lwrc$(menu$) = "codeexplore" then ! #Case# "codeexplore"
48588       let fncodeexplore(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48590     else if lwrc$(menu$) = "orphanedfunctions" then ! #Case# "orphanedfunctions"
48592       let fnorphanedfunctions
48594     else if lwrc$(menu$) = "generatecode" then ! #Case# "generatecode"
48596       let fngeneratecode(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48598     else if lwrc$(menu$) = "generatescreen" then ! #Case# "generatescreen"
48600       if fnoktoproceed(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
48602         let fngeneratescreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48604       end if 
48606     else if lwrc$(menu$) = "drawdots" then ! #Case# "drawdots"
48608       let settingtempdots=1
48610       let dotsprinted=0
48612       let fnprocesswindowsmenu=menumovement
48614     else ! #Case Else#
48616       input menu status: mat status$
48618       if menu and menu<=udim(mat status$) then 
48620         if pos(uprc$(status$(menu)),"C") then 
48622           if pos(uprc$(status$(menu)),"X") then 
48624             let setenv(menu$,"Y")
48626           else 
48628             let setenv(menu$,"N")
48630           end if 
48632         end if 
48634       end if 
48636     end if  ! #End Select#
48638   fnend 
48640 ! 
48642 DEFINEBUTTONS: ! Define Toolbar Buttons
48644   def fndefinebuttons(mat buttons$, mat tooltip$)
48646     mat buttons$(14) : mat tooltip$(14)
48648 ! 
48650     let buttons$(1)="Field"
48652     let tooltip$(1)="Add the selected Database Field"
48654 ! 
48656     let buttons$(2)="Text Box"
48658     let tooltip$(2)="Add a blank Text Box field"
48660 ! 
48662     let buttons$(3)="Caption"
48664     let tooltip$(3)="Add an empty Caption"
48666 ! 
48668     let buttons$(4)="Check Box"
48670     let tooltip$(4)="Add a blank Check Box field"
48672 ! 
48674     let buttons$(5)="Combo Box"
48676     let tooltip$(5)="Add a combo box control."
48678 ! 
48680     let buttons$(6)="Listview"
48682     let tooltip$(6)="Add a listview control"
48684 ! 
48686     let buttons$(7)="Search Box"
48688     let tooltip$(7)="Add a Search Box to a Listview Control"
48690 ! 
48692     let buttons$(8)="Filter Box"
48694     let tooltip$(8)="Add a BR filter box. (4.3+)"
48696 ! 
48698     let buttons$(9)="Button"
48700     let tooltip$(9)="Add a Button"
48702 ! 
48704     let buttons$(10)="Picture"
48706     let tooltip$(10)="Add a Picture"
48708 ! 
48710     let buttons$(11)="~ Calendar ~"
48712     let tooltip$(11)="Add button that links to a date picker.(Coming Soon)"
48714 ! 
48716     let buttons$(12)="Frame"
48718     let tooltip$(12)="Add a border for visual grouping."
48720 ! 
48722     let buttons$(13)="Screen"
48724     let tooltip$(13)="Add an entire screen as a child control."
48726 ! 
48728     let buttons$(14)="Skip a Space"
48730     let tooltip$(14)="Skip the next row for Automatic Placement"
48732 ! 
48734     let fnfixtooltips(mat tooltip$)
48736   fnend 
48738 ! 
48740 PROCESSBUTTONS: ! Process A Button Click Here
48742   def fnprocessbutton(button$,&mode,&control,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,newcontrol,relatedcontrol)
48744     if button$ = "field" then ! #Select# Button$ #Case# "field"
48746       let newcontrol=fnaddandcolorcurrentfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat screenio$,mat screenio)
48748       if newcontrol then 
48750         let mode=inputeditormovemode
48752         let control=newcontrol
48754       end if 
48756 ! 
48758     else if button$ = "text box" then ! #Case# "text box"
48760       let newcontrol=fnaddtextfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48762       if newcontrol then 
48764         let mode=inputeditormovemode
48766         let control=newcontrol
48768         let fgcolor$(newcontrol)="W"
48770         let bgcolor$(newcontrol)="W"
48772       end if 
48774 ! 
48776     else if button$ = "caption" then ! #Case# "caption"
48778       let newcontrol=fnaddcaption(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48780       if newcontrol then 
48782         let mode=inputeditormovemode
48784         let control=newcontrol
48786         if trim$(screenio$(si_fgcolor))<>"" then 
48788           let fgcolor$(newcontrol)=trim$(screenio$(si_fgcolor))
48790         end if 
48792       end if 
48794 ! 
48796     else if button$ = "check box" then ! #Case# "check box"
48798       let newcontrol=fnaddcheckbox(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48800       if newcontrol then 
48802         let mode=inputeditormovemode
48804         let control=newcontrol
48806       end if 
48808       if trim$(screenio$(si_fgcolor))<>"" then 
48810         let fgcolor$(newcontrol)=trim$(screenio$(si_fgcolor))
48812       end if 
48814 ! 
48816     else if button$ = "search box" then ! #Case# "search box"
48818       let newcontrol=fnaddsearchbox(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mode,control)
48820       if newcontrol then 
48822         let mode=inputeditormovemode
48824         let control=newcontrol
48826         let fgcolor$(newcontrol)="W"
48828         let bgcolor$(newcontrol)="W"
48830       end if 
48832 ! 
48834     else if button$ = "filter box" then ! #Case# "filter box"
48836       let newcontrol=fnaddfilterbox(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mode,control)
48838       if newcontrol then 
48840         let mode=inputeditormovemode
48842         let control=newcontrol
48844         let fgcolor$(newcontrol)="W"
48846         let bgcolor$(newcontrol)="W"
48848       end if 
48850 ! 
48852     else if button$ = "calendar" then ! #Case# "calendar"
48854       let newcontrol=fnaddcalendar(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mode,control)
48856       if newcontrol then 
48858         let mode=inputeditormovemode
48860         let control=newcontrol
48862         let fgcolor$(newcontrol)="W"
48864         let bgcolor$(newcontrol)="W"
48866       end if 
48868 ! 
48870     else if button$ = "listview" then ! #Case# "listview"
48872       let newcontrol=fnaddlistview(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48874       if newcontrol then 
48876         let mode=inputeditormovemode
48878         let control=newcontrol
48880         let fgcolor$(newcontrol)="W"
48882         let bgcolor$(newcontrol)="W"
48884       end if 
48886 ! 
48888     else if button$ = "button" then ! #Case# "button"
48890       let newcontrol=fnaddbutton(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48892       if newcontrol then 
48894         let mode=inputeditormovemode
48896         let control=newcontrol
48898         let fgcolor$(newcontrol)="W"
48900         let bgcolor$(newcontrol)="W"
48902       end if 
48904 ! 
48906     else if button$ = "picture" then ! #Case# "picture"
48908       let newcontrol=fnaddpicture(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48910       if newcontrol then 
48912         let mode=inputeditormovemode
48914         let control=newcontrol
48916       end if 
48918     else if button$ = "screen" then ! #Case# "screen"
48920       let newcontrol=fnaddscreen(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48922       if newcontrol then 
48924         let mode=inputeditormovemode
48926         let control=newcontrol
48928       end if 
48930 ! 
48932     else if button$ = "frame" then ! #Case# "frame"
48934       let newcontrol=fnaddframe(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48936       if newcontrol then 
48938         let mode=inputeditormovemode
48940         let control=newcontrol
48942       end if 
48944 ! 
48946     else if button$ = "combo box" then ! #Case# "combo box"
48948       let newcontrol=fnaddcombo(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
48950       if newcontrol then 
48952         let mode=inputeditormovemode
48954         let control=newcontrol
48956       end if 
48958 ! 
48960     else if button$ = "skip a space" then ! #Case# "skip a space"
48962       let fnskipagap
48964 ! 
48966     else ! #Case Else#
48968       let msgbox("This control type is not supported yet.","Control not supported","Ok","INF")
48970 ! 
48972     end if  ! #End Select#
48974   fnend 
48976 ! 
48978   def fnaddtoclipboard(&clipboard$,control,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,child)
48980     let clipboard$=clipboard$&trim$(controlname$(control))&"_#_"
48982     let clipboard$=clipboard$&trim$(fieldname$(control))&"_#_"
48984     let clipboard$=clipboard$&trim$(description$(control))&"_#_"
48986     let clipboard$=clipboard$&str$(vposition(control))&"_#_"
48988     let clipboard$=clipboard$&str$(hposition(control))&"_#_"
48990     let clipboard$=clipboard$&trim$(fieldtype$(control))&"_#_"
48992     let clipboard$=clipboard$&str$(specwidth(control))&"_#_"
48994     let clipboard$=clipboard$&str$(width(control))&"_#_"
48996     let clipboard$=clipboard$&str$(height(control))&"_#_"
48998     let clipboard$=clipboard$&trim$(truevalue$(control))&"_#_"
49000     let clipboard$=clipboard$&trim$(falsevalue$(control))&"_#_"
49002     let clipboard$=clipboard$&trim$(function$(control))&"_#_"
49004     let clipboard$=clipboard$&trim$(picture$(control))&"_#_"
49006     let clipboard$=clipboard$&trim$(parent$(control))&"_#_"
49008     let clipboard$=clipboard$&trim$(fgcolor$(control))&"_#_"
49010     let clipboard$=clipboard$&trim$(bgcolor$(control))&"_#_"
49012     let clipboard$=clipboard$&trim$(justify$(control))&"_#_"
49014     let clipboard$=clipboard$&trim$(attr$(control))&"_#_"
49016     let clipboard$=clipboard$&str$(multiselect(control))&"_#_"
49018     let clipboard$=clipboard$&str$(gridlines(control))&"_#_"
49020     let clipboard$=clipboard$&str$(protected(control))&"_#_"
49022     let clipboard$=clipboard$&str$(invisible(control))&"_#_"
49024     let clipboard$=clipboard$&trim$(tooltip$(control))&"_#_"
49026     let clipboard$=clipboard$&trim$(cnvrtin$(control))&"_#_"
49028     let clipboard$=clipboard$&trim$(cnvrtout$(control))&"_#_"
49030     let clipboard$=clipboard$&trim$(userdata$(control))&"_#_"
49032     let clipboard$=clipboard$&"#!!#"&chr$(13)
49034 ! 
49036 ! .   ! Add listview children to clipboard also.
49038     if lwrc$(fieldtype$(control))="listview" then 
49040       for child=1 to udim(mat fieldtype$)
49042         if parent$(child)=parent$(control) and lwrc$(fieldtype$(child))="listchld" then 
49044           let fnaddtoclipboard(clipboard$,child,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
49046         end if 
49048       next child
49050     end if 
49052   fnend 
49054   def fnaddcontrolsfromclipboard(&clipboard$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,added)
49056     let clipboard$=srep$(clipboard$,chr$(13),"") ! Strip Cr's
49058     do while pos(clipboard$,"#!!#")
49060 ! .      ! Read one control and add it to screen
49062       let added+=fnaddcontrolfromclip(clipboard$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
49064 ! .      ! Strip it from Clipboard
49066       if pos(clipboard$,"#!!#") then 
49068         let clipboard$(1:pos(clipboard$,"#!!#")+3)=""
49070       end if 
49072     loop 
49074     if added then let fnaddcontrolsfromclipboard=1
49076   fnend 
49078   def fnaddcontrolfromclip(&clipboard$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,controlname$*50,fieldname$*50,description$*255,vposition,hposition,fieldtype$,specwidth,width,height,truevalue$*60,falsevalue$*60,function$*255,picture$*255,parent$*20,fgcolor$,bgcolor$,protected,invisible,tooltip$*255,cnvrtin$*255,cnvrtout$*255,multiselect,gridlines,userdata$*255,justify$,attr$*255,control)
49080     let broken=0
49082     if ~broken then let controlname$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49084     if ~broken then let fieldname$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49086     if ~broken then let description$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49088     if ~broken then let vposition=val(fngetclip$(broken,clipboard$,"_#_")) conv BADCLIPBOARD
49090     if ~broken then let hposition=val(fngetclip$(broken,clipboard$,"_#_")) conv BADCLIPBOARD
49092     if ~broken then let fieldtype$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49094     if ~broken then let specwidth=val(fngetclip$(broken,clipboard$,"_#_")) conv BADCLIPBOARD
49096     if ~broken then let width=val(fngetclip$(broken,clipboard$,"_#_")) conv BADCLIPBOARD
49098     if ~broken then let height=val(fngetclip$(broken,clipboard$,"_#_")) conv BADCLIPBOARD
49100     if ~broken then let truevalue$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49102     if ~broken then let falsevalue$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49104     if ~broken then let function$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49106     if ~broken then let picture$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49108     if ~broken then let parent$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49110     if ~broken then let fgcolor$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49112     if ~broken then let bgcolor$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49114     if ~broken then let justify$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49116     if ~broken then let attr$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49118     if ~broken then let multiselect=val(fngetclip$(broken,clipboard$,"_#_")) conv BADCLIPBOARD
49120     if ~broken then let gridlines=val(fngetclip$(broken,clipboard$,"_#_")) conv BADCLIPBOARD
49122     if ~broken then let protected=val(fngetclip$(broken,clipboard$,"_#_")) conv BADCLIPBOARD
49124     if ~broken then let invisible=val(fngetclip$(broken,clipboard$,"_#_")) conv BADCLIPBOARD
49126     if ~broken then let tooltip$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49128     if ~broken then let cnvrtin$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49130     if ~broken then let cnvrtout$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49132     if ~broken then let userdata$=fngetclip$(broken,clipboard$,"_#_") soflow BADCLIPBOARD
49134     if ~broken then 
49136       let control=fnaddemptyfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
49138       let controlname$(control)=controlname$
49140       let fieldname$(control)=fieldname$
49142       let description$(control)=description$
49144       let vposition(control)=vposition
49146       let hposition(control)=hposition
49148       let fieldtype$(control)=fieldtype$
49150       let specwidth(control)=specwidth
49152       let width(control)=width
49154       let height(control)=height
49156       let truevalue$(control)=truevalue$
49158       let falsevalue$(control)=falsevalue$
49160       let function$(control)=function$
49162       let picture$(control)=picture$
49164       let parent$(control)=parent$
49166       let fgcolor$(control)=fgcolor$
49168       let bgcolor$(control)=bgcolor$
49170       let justify$(control)=justify$
49172       let attr$(control)=attr$
49174       let protected(control)=protected
49176       let invisible(control)=invisible
49178       let tooltip$(control)=tooltip$
49180       let cnvrtin$(control)=cnvrtin$
49182       let cnvrtout$(control)=cnvrtout$
49184       let multiselect(control)=multiselect
49186       let gridlines(control)=gridlines
49188       let userdata$(control)=userdata$
49190       let fnaddcontrolfromclip=1
49192     else 
49194       print bell;
49196     end if 
49198   fnend 
49200 ! 
49202   def fngetclip$*255(&broken,&clipboard$,terminator$)
49204     if pos(clipboard$,terminator$)>0 then 
49206       let fngetclip$=trim$(clipboard$(1:pos(clipboard$,terminator$)-1)) soflow BADCLIPBOARD
49208       let clipboard$(1:pos(clipboard$,terminator$)+len(terminator$)-1)=""
49210     else 
49212       let broken=1
49214     end if 
49216   fnend 
49218 BADCLIPBOARD: let broken=1 : continue 
49220 ! 
49222 ! 
49224 DEFINEATTRIBUTESWINDOW: !  Define Window Attributes Input Fields
49226   def fndefineattributeswindow(mat attributecaptions$, mat attributecapspec$,mat attributespec$,mat attributedata$,mat attributettip$,mat attributesubs;___,index,ttlev)
49228 ! 
49230     mat attributecaptions$(19) : mat attributecapspec$(19)
49232     mat attributespec$(19) : mat attributedata$(19)
49234     mat attributettip$(19) : mat attributesubs(19)
49236 ! 
49238     let attributecaptions$(1)= "Window Name:"
49240     let attributecapspec$(1)= "1,1,C  12"
49242     let attributespec$(1)= "1,16,9/CU 18,S,1101"
49244     let attributesubs(1)= si_screencode
49246     let attributettip$(1)="The name of the screen, required in order to save the screen."
49248 ! 
49250     let attributecaptions$(2)= "Caption:"
49252     let attributecapspec$(2)= "2,1,C 8"
49254     let attributespec$(2)= "2,9,16/V 60,S,1102"
49256     let attributesubs(2)= si_caption
49258     let attributettip$(2)="Caption to display in the windows taskbar when running screen."
49260 ! .    !
49262     let attributecaptions$(3)= "Rows:"
49264     let attributecapspec$(3)= "3,1,C 5"
49266     let attributespec$(3)= "3,8,5/CR 3,S,1103"
49268     let attributesubs(3)=-1*si_vsize
49270     let attributettip$(3)="Height of screen in Rows."
49272 ! 
49274     let attributecaptions$(4)= "Cols:"
49276     let attributecapspec$(4)= "3,14,C 5"
49278     let attributespec$(4)= "3,20,5/CR 3,S,1104"
49280     let attributesubs(4)=-1*si_hsize
49282     let attributettip$(4)="Width of screen in columns."
49284 ! 
49286     let attributecaptions$(5)= "Attributes:"
49288     let attributecapspec$(5)= "4,1,C 11"
49290     let attributespec$(5)= "4,12,13/V 255,S,1105"
49292     let attributesubs(5)=si_attributes
49294     let attributettip$(5)="Attributes to use when opening the window"
49296 ! 
49298     let attributecaptions$(6)= "Border:"
49300     let attributecapspec$(6)= "5,1,6/C 7"
49302     let attributespec$(6)= "5,7,CHECK 1,,1106"
49304     let attributesubs(6)=-1*si_border
49306     let attributettip$(6)="Check to add a window border."
49308 ! 
49310     let attributecaptions$(7)= "Cap:"
49312     let attributecapspec$(7)= "5,9,C 4"
49314     let attributespec$(7)= "5,13,12/V 80,S,1107"
49316     let attributesubs(7)=si_windcap
49318     let attributettip$(7)="Caption to place on the border, if a border is checked."
49320 ! 
49322     let attributecaptions$(8)= "Picture:"
49324     let attributecapspec$(8)= "6,1,C 8"
49326     let attributespec$(8)= "6,9,16/V 60,S,1108"
49328     let attributesubs(8)=si_picture
49330     let attributettip$(8)="Background image for the window."
49332 ! 
49334     let attributecaptions$(9)= "Read Key:"
49336     let attributecapspec$(9)= "7,1,7/C 9"
49338     let attributespec$(9)= "7,9,CR 3,S,1109"
49340     let attributesubs(9)=-1*si_readindex
49342     let attributettip$(9)="Here you indicate which key to use when reading the file. The default is 0 (relative) for fast reading of listviews, and 1 for add/edit screens."
49344 ! 
49346     let attributecaptions$(10)= "Return Key:"
49348     let attributecapspec$(10)= "7,13,8/C 11"
49350     let attributespec$(10)= "7,22,CR 3,S,1110"
49352     let attributesubs(10)=-1*si_returnindex
49354     let attributettip$(10)="Which key to use when returning the user action to the calling program. Fnfm will return the selected or edited records key (using this number to say which key) as its return value."
49356 ! 
49358     let attributecaptions$(11)= "Input Attr:"
49360     let attributecapspec$(11)= "8,1,C 11"
49362     let attributespec$(11)= "8,12,13/V 255,S,1111"
49364     let attributesubs(11)=si_inputattr
49366     let attributettip$(11)="This is the attribute that ScreenIO will use for the active field, the field that the cursor is currently in."
49368 ! 
49370     let attributecaptions$(12)= "Wait Time:"
49372     let attributecapspec$(12)= "9,1,C 10"
49374     let attributespec$(12)= "9,20,5/CR 3,S,1112"
49376     let attributesubs(12)=-1*si_waittime
49378     let attributettip$(12)="If you specify a wait time here, screenio will timeout and call your wait function after the specified time of inactivity in your screen."
49380 ! 
49382     let attributecaptions$(13)= "Dont Lock:"
49384     let attributecapspec$(13)= "10,1,9/C 10"
49386     let attributespec$(13)= "10,10,CHECK 1,,1113"
49388     let attributesubs(13)=-1*si_screeniolocking
49390     let attributettip$(13)="Check this box to force the screen to never lock the records, even on editing. If the user attempts to save data and the record wasn't locked, ScreenIO will attempt to do its own record locking to resolve any conflicts."
49392 ! 
49394     let attributecaptions$(14)= "Auto Merge:"
49396     let attributecapspec$(14)= "10,12,10/C 11"
49398     let attributespec$(14)= "10,22,CHECK 1,,1114"
49400     let attributesubs(14)=-1*si_screeniomerge
49402     let attributettip$(14)="Check this box for screenio to automatically attempt to merge data based on timestamp any time record locking is disabled and two users change the same record. If this is unchecked, a guided merge will be performed using ""postit notes""."
49404 ! 
49406     let attributecaptions$(15)= "Color:. /"
49408     let attributecapspec$(15)= "11,1,8/C 14"
49410     let attributespec$(15)= "11,10,C 6,/W:W,B1115"
49412     let attributesubs(15)=si_fgcolor
49414     let attributettip$(15)="Foreground color to use for the screen, and for the default for newly added text controls."
49416 ! 
49418     let attributecaptions$(16)= ":"
49420     let attributecapspec$(16)= "11,16,CC 2"
49422     let attributespec$(16)= "11,18,C 6,/W:W,B1116"
49424     let attributesubs(16)=si_bgcolor
49426     let attributettip$(16)="Background color to use for the screen, and for the default for newly added textbox controls."
49428 ! 
49430     let attributecaptions$(17)= "File Layout:"
49432     let attributecapspec$(17)= "12,1,11/C 12"
49434     let attributespec$(17)= "12,12,12/CC 18,/W:W,B1117"
49436     let attributesubs(17)=si_filelay
49438     let attributettip$(17)="Select the file layout, to tie your screen directly to a data file."
49440 ! 
49442     let attributecaptions$(18)= ""
49444     let attributecapspec$(18)= "13,1,C 1"
49446     let attributespec$(18)= "13,2,10/CC 17,/W:W,B1118"
49448     let attributedata$(18)= "Set Screen Events"
49450     let attributesubs(18)= 0
49452     let attributettip$(18)="Set the window level event functions here."
49454 ! 
49456     let attributecaptions$(19)= ""
49458     let attributecapspec$(19)= "13,2,C 1"
49460     let attributespec$(19)= "13,14,10/CC 15,/W:W,B1119"
49462     let attributedata$(19)= "Set Tab Order"
49464     let attributesubs(19)= 0
49466     let attributettip$(19)="Use this button to change the tab order."
49468 ! 
49470     let fnfixtooltips(mat attributettip$)
49472 ! 
49474   fnend 
49476 ! 
49478 ! =============================
49480 ! =    End Function Blocks    =
49482 ! =============================
49484 ! 
49486 DISPLAYWINDOWSMENU: !  Display The Windows Menu
49488   def fndisplaywindowsmenu(;___,index)
49490     dim m$(1)*30, pgm$(1), status$(1)
49492     let fndefinewindowsmenu(mat m$,mat pgm$, mat status$)
49494 ! 
49496     display menu: mat m$, mat pgm$, mat status$
49498   fnend 
49500 ! 
49502 CLEARWINDOWSMENU: ! Display The Windows Menu
49504   def fnclearwindowsmenu
49506     dim mclear$(1), pgmclear$(1), statusclear$(1)
49508 ! 
49510     mat mclear$(0) : mat pgmclear$(0) : mat statusclear$(0)
49512 ! 
49514     display menu: mat mclear$, mat pgmclear$, mat statusclear$
49516   fnend 
49518 ! 
49520 SHOWABOUT: !   *****  This Shows  The About Page
49522   def fnshowabout(;___,about, cover, x$, function )
49524     open #(about:=fngetfilenumber) : "srow=5,scol=20,rows=23,cols=60,Border=S/#FFFFFF:#000000", display,outin 
49526     print #about, fields "2,8,P 7/18,,35" : setting_imagepath$&"\sageax.bmp"
49528     print #about, fields "2,30,CC 30" : "Screen IO Library - Designer"
49530     print #about, fields "4,30,CC 30" : "Copyright 2008 Gabriel Bakker"
49532     print #about, fields "6,30,CC 30" : "Sage AX"
49534     print #about, fields "7,30,CC 30,U/#0000FF:W,35" : "http://www.sageax.com"
49536     print #about, fields "10,2,CC 58" : "This library was designed as an addon for FileIO"
49538     print #about, fields "11,2,CC 58" : "to be an all in one program generation system."
49540     print #about, fields "13,2,CC 58" : "Special thanks to Chris Shields for helping to salvage"
49542     print #about, fields "14,2,CC 58" : "the source code after a disasterous theft."
49544     print #about, fields "16,2,CC 58" : "Special thanks to Susan Smith for support, testing,"
49546     print #about, fields "17,2,CC 58" : "ideas and extensive encouragement during development."
49548     print #about, fields "19,2,CC 58" : "Special thanks to Mikhail Zheleznov for encouragement,"
49550     print #about, fields "20,2,CC 58" : "enthusiasm, many new ideas, and hours of testing."
49552     print #about, fields "22,15,CC 10,/W:W,B37;22,35,CC 10,/W:W,B38" : "OK","Thank You"
49554     print #about, fields "23,40,CR 20" : "Version 2.0"
49556     open #(cover:=fngetfilenumber) : "srow=12,scol=2,rows=1,cols=3,parent="&str$(about),display, outin 
49558     do 
49560       let x$=""
49562       rinput #about, fields "12,3,CU 1,AEX" : x$
49564       let function=fkey
49566       if function = 35 then ! #Select# Function #Case# 35
49568         execute "system -c -M start http://www.sageax.com"
49570       else if function = 38 or x$="S" then ! #Case# 38 or X$="S"
49572         let fnsupporters
49574       end if  ! #End Select#
49576     loop until function=37 or function=99 or function=0 or fkey=93 or function=93 ! Esc,  Enter,  Or Ok Button or exit button
49578     close #cover: 
49580     close #about: 
49582   fnend 
49584 ! 
49586   dim supporterspec$(8)*32, supporterout$(8)*64, supportername$(36)*64
49588 ! 
49590 SUPPORTERS: !   *****  This Shows  The About Page
49592   def fnsupporters(;___,about,x$,function,i,smoothscroll,counter,nameindex,displayfrom,suppressnames,index,looping)
49594     let supportername$(1)="Susan Smith"
49596     let supportername$(2)="SMS Software"
49598     let supportername$(3)=""
49600     let supportername$(4)="Bart Fusco"
49602     let supportername$(5)="Foxtree Integrated Pest Management"
49604     let supportername$(6)=""
49606     let supportername$(7)="Percy J Comeaux"
49608     let supportername$(8)="Erisa"
49610     let supportername$(9)=""
49612     let supportername$(10)="George Tisdale"
49614     let supportername$(11)="Tisdale Certified Public Accountants"
49616     let supportername$(12)=""
49618     let supportername$(13)="Luis Gomez and John Curry"
49620     let supportername$(14)="Commercial Legal Software"
49622     let supportername$(15)=""
49624     let supportername$(16)="Pete Klug"
49626     let supportername$(17)="Commercial Testing Laboratory"
49628     let supportername$(18)=""
49630     let supportername$(19)="Richard Forrester"
49632     let supportername$(20)="Sys-Corp Services"
49634     let supportername$(21)=""
49636     let supportername$(22)="Mike Storlazzi"
49638     let supportername$(23)="Payrolls Unlimited"
49640     let supportername$(24)=""
49642     let supportername$(25)="Doug Meenen and Stephen Koger"
49644     let supportername$(26)="Management Advisory Computer Systems"
49646     let supportername$(27)=""
49648     let supportername$(28)="and of course"
49650     let supportername$(29)="Gordon Dye and Gary Hoff for their"
49652     let supportername$(30)=""
49654     let supportername$(31)="hard work and dedication to the Business Rules language,"
49656     let supportername$(32)="which has kept us all in business doing what we love."
49658     let supportername$(33)=""
49660     let supportername$(34)="Without Business Rules, there would be no ScreenIO."
49662     let supportername$(35)=" "
49664     let supportername$(36)=""
49666 ! 
49668 ! 
49670     let smoothscroll=fn43
49672     execute "CONFIG ATTRIBUTE [SUPPORTERS] ,font=Arial:Large:Slant"
49674     open #(about:=fngetfilenumber) : "srow=9,scol=26,rows=21,cols=60,FONT.LABELS=Comic Sans MS:bold:slant,Border=S", display,outin 
49676     print #about, fields "1,2,CC 58,[SUPPORTERS]" : "Special Thanks to all the early adopters of ScreenIO"
49678     print #about, fields "21,2,CC 58,[SUPPORTERS]" : "Without you, this would never have been possible."
49680 ! 
49682     let displayfrom=1
49684 ! .   ! let SuppressNames=1
49686 ! 
49688     do 
49690       if counter=0 then 
49692         let counter=20
49694         let nameindex+=1
49696         if nameindex=37 then 
49698 ! .            ! End of the list. Pause, then restart.
49700           let fnpause(2,x$,function)
49702           let nameindex=1
49704           let displayfrom=1
49706           let increment=0
49708           let suppressnames=7
49710         end if 
49712         if suppressnames then let suppressnames-=1
49714       end if 
49716       if smoothscroll then let counter-=1 else let counter-=10
49718 ! 
49720       if counter=0 then 
49722         if displayfrom<=udim(supporterout$) and supporterout$(displayfrom)><"" then 
49724           if mod(displayfrom,2)=1 then ! Scrunch up together at the top
49726             let increment-=1
49728             let counter+=10
49730           end if 
49732           let displayfrom+=1
49734         end if 
49736       end if 
49738 ! 
49740       if displayfrom>8 then 
49742         let displayfrom=1
49744         let increment=0
49746 ! .         ! End of page. Pause, set SuppressNames to clear, and cont.
49748         let fnpause(2,x$,function)
49750         let suppressnames=udim(mat supporterout$)
49752       end if 
49754 ! 
49756 ! .      ! Calculate the specs
49758       for i=4 to 18 step 2
49760         let supporterspec$((i/2)-1)=str$(i+increment+(counter/10))&",1,CC 60"
49762       next i
49764 ! 
49766 ! .      ! Calculate the names
49768       mat supporterout$=("")
49770       if nameindex<=8 then 
49772         mat supporterout$(8-nameindex+1:8)=supportername$(1:nameindex)
49774       else 
49776         mat supporterout$(1:8)=supportername$(nameindex-8+1:nameindex)
49778       end if 
49780 ! 
49782 ! .      ! Apply SuppresssNames to clear screen of prev. page
49784       for index=1 to udim(mat supporterout$)
49786         if index<=suppressnames then 
49788           let supporterout$(index)=""
49790         end if 
49792       next index
49794 ! 
49796 ! .      ! Print
49798 ! .      ! print #About, fields "1,1,N 3;2,1,N 3" : DisplayFrom,Increment   ! Debug output
49800       print #about, fields mat supporterspec$(displayfrom:udim(mat supporterout$)) : mat supporterout$(displayfrom:udim(mat supporterout$))
49802 ! 
49804 ! .      ! Pause
49806       if function><99 and unhex$(x$)><"6300" and x$><" " then 
49808         if smoothscroll then 
49810           let fnpause(.05,x$,function)
49812         else 
49814           let fnpause(.5,x$,function)
49816         end if 
49818       end if 
49820 ! 
49822 ! .      ! Erase
49824       if ~smoothscroll then 
49826         mat supporterout$=("")
49828         print #about, fields mat supporterspec$(displayfrom:udim(mat supporterout$)) : mat supporterout$(displayfrom:udim(mat supporterout$))
49830       end if 
49832 ! 
49834 ! .      ! Loop
49836     loop until function=99 or function=93 or unhex$(x$)="6300" or x$=" " ! Press Esc or space bar
49838     close #about: 
49840   fnend 
49842 ! 
49844   def fnpause(howlong;&thekey$,&function,___,looping)
49846     do 
49848       let sleep(.05)
49850       let x$=kstat$
49852       let function=fkey
49854       let looping+=.05
49856     loop until looping>=howlong or function=99 or unhex$(x$)="6300"
49858   fnend 
49860 ! 
49862 OKTOPROCEED: !   ***** Ask The User If They Are Sure  They Want  To Proceed
49864   def fnoktoproceed(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,choice,yes,no,cancel)
49866     if (~fncheckscreenarrays(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)) then 
49868       let yes=2 : let no=3 : let cancel=4 ! Msgbox Return Constants
49870       let choice=msgbox("Warning: If you proceed you will overwrite all changes. Would you like to save your changes first?","Save Changes?","Ync","QST")
49872       if choice = yes then ! #Select# Choice #Case# Yes
49874         if fnwritescreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
49876           let fnoktoproceed=1
49878         else 
49880           let fnoktoproceed=0
49882         end if 
49884       else if choice = no then ! #Case# No
49886         let fnoktoproceed=1
49888       else if choice = cancel then ! #Case# Cancel
49890         let fnoktoproceed=0
49892       else ! #Case Else#
49894         let fnoktoproceed=0
49896       end if  ! #End Select#
49898     else 
49900       let fnoktoproceed=1
49902     end if 
49904   fnend 
49906 ! 
49908   dim p_screenio$(1)*255,p_screenio(1)
49910   dim p_controlname$(1)*50, p_fieldname$(1)*50, p_description$(1)*255
49912   dim p_vposition(1), p_hposition(1), p_fieldtype$(1)*8, p_specwidth(1), p_width(1), p_height(1)
49914   dim p_truevalue$(1)*60, p_falsevalue$(1)*60
49916   dim p_function$(1)*255, p_picture$(1)*255, p_parent$(1)*20
49918   dim p_fgcolor$(1)*6, p_bgcolor$(1)*6, p_justify$(1)*1, p_attr$(1)*128, p_multiselect(1),p_gridlines(1)
49920   dim p_protected(1), p_invisible(1), p_tooltip$(1)*255,p_cnvrtin$(1)*255,p_cnvrtout$(1)*255, p_userdata$(1)*255
49922 ! 
49924 PRESERVESCREENARRAYS: !  Preserves The State Of The Screen Arrays
49926   def fnpreservescreenarrays(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
49928 ! 
49930     mat p_screenio$(udim(mat screenio$)) = screenio$
49932     mat p_screenio(udim(mat screenio)) = screenio
49934     mat p_controlname$(udim(mat controlname$)) = controlname$
49936     mat p_fieldname$(udim(mat fieldname$)) = fieldname$
49938     mat p_description$(udim(mat description$)) = description$
49940     mat p_vposition(udim(mat vposition)) = vposition
49942     mat p_hposition(udim(mat hposition)) = hposition
49944     mat p_fieldtype$(udim(mat fieldtype$)) = fieldtype$
49946     mat p_specwidth(udim(mat specwidth)) = specwidth
49948     mat p_width(udim(mat width)) = width
49950     mat p_height(udim(mat height)) = height
49952     mat p_truevalue$(udim(mat truevalue$)) = truevalue$
49954     mat p_falsevalue$(udim(mat falsevalue$)) = falsevalue$
49956     mat p_function$(udim(mat function$)) = function$
49958     mat p_picture$(udim(mat picture$)) = picture$
49960     mat p_parent$(udim(mat parent$)) = parent$
49962     mat p_fgcolor$(udim(mat fgcolor$)) = fgcolor$
49964     mat p_bgcolor$(udim(mat bgcolor$)) = bgcolor$
49966     mat p_justify$(udim(mat justify$)) = justify$
49968     mat p_attr$(udim(mat attr$)) = attr$
49970     mat p_protected(udim(mat protected)) = protected
49972     mat p_invisible(udim(mat invisible)) = invisible
49974     mat p_tooltip$(udim(mat tooltip$)) = tooltip$
49976     mat p_cnvrtin$(udim(mat cnvrtin$)) = cnvrtin$
49978     mat p_cnvrtout$(udim(mat cnvrtout$)) = cnvrtout$
49980     mat p_multiselect(udim(mat multiselect)) = multiselect
49982     mat p_gridlines(udim(mat gridlines)) = gridlines
49984     mat p_userdata$(udim(mat userdata$)) = userdata$
49986   fnend 
49988 ! 
49990 CHECKSCREENARRAYS: !  Check The State Of The Screen Arrays
49992   def fncheckscreenarrays(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,failed)
49994 ! 
49996     let failed=failed or (~fnsameas(mat screenio$,mat p_screenio$))
49998     let failed=failed or (~fnsamea(mat screenio,mat p_screenio))
50000     let failed=failed or (~fnsameas(mat controlname$,mat p_controlname$))
50002     let failed=failed or (~fnsameas(mat fieldname$,mat p_fieldname$))
50004     let failed=failed or (~fnsameas(mat description$,mat p_description$))
50006     let failed=failed or (~fnsamea(mat vposition,mat p_vposition))
50008     let failed=failed or (~fnsamea(mat hposition,mat p_hposition))
50010     let failed=failed or (~fnsameas(mat fieldtype$,mat p_fieldtype$))
50012     let failed=failed or (~fnsamea(mat specwidth,mat p_specwidth))
50014     let failed=failed or (~fnsamea(mat width,mat p_width))
50016     let failed=failed or (~fnsamea(mat height,mat p_height))
50018     let failed=failed or (~fnsameas(mat truevalue$,mat p_truevalue$))
50020     let failed=failed or (~fnsameas(mat falsevalue$,mat p_falsevalue$))
50022     let failed=failed or (~fnsameas(mat function$,mat p_function$))
50024     let failed=failed or (~fnsameas(mat picture$,mat p_picture$))
50026     let failed=failed or (~fnsameas(mat parent$,mat p_parent$))
50028     let failed=failed or (~fnsameas(mat fgcolor$,mat p_fgcolor$))
50030     let failed=failed or (~fnsameas(mat bgcolor$,mat p_bgcolor$))
50032     let failed=failed or (~fnsameas(mat justify$,mat p_justify$))
50034     let failed=failed or (~fnsameas(mat attr$,mat p_attr$))
50036     let failed=failed or (~fnsamea(mat protected,mat p_protected))
50038     let failed=failed or (~fnsamea(mat protected,mat p_protected))
50040     let failed=failed or (~fnsameas(mat tooltip$,mat p_tooltip$))
50042     let failed=failed or (~fnsameas(mat cnvrtin$,mat p_cnvrtin$))
50044     let failed=failed or (~fnsameas(mat cnvrtout$,mat p_cnvrtout$))
50046     let failed=failed or (~fnsamea(mat multiselect,mat p_multiselect))
50048     let failed=failed or (~fnsamea(mat gridlines,mat p_gridlines))
50050     let failed=failed or (~fnsameas(mat userdata$,mat p_userdata$))
50052     let fncheckscreenarrays=(~failed)
50054   fnend 
50056 ! 
50058 ! .! Change the width without marking the screen data as changed.
50060   def fnchangespecwidth(index,width)
50062     let specwidth(index)=width
50064     if udim(mat specwidth)=udim(mat p_specwidth) then 
50066       let p_specwidth(index)=width
50068     end if 
50070 ! .   ! if the sizes dont match then we're changed anyway.
50072     if udim(mat specwidth)=udim(mat undo_specwidth) then 
50074       let undo_specwidth(index)=width
50076     end if 
50078   fnend 
50080 ! 
50082   dim undo_screenio$(1)*255,undo_screenio(1)
50084   dim undo_controlname$(1)*50, undo_fieldname$(1)*50, undo_description$(1)*255
50086   dim undo_vposition(1), undo_hposition(1), undo_fieldtype$(1)*8, undo_specwidth(1), undo_width(1), undo_height(1)
50088   dim undo_truevalue$(1)*60, undo_falsevalue$(1)*60
50090   dim undo_function$(1)*255, undo_picture$(1)*255, undo_parent$(1)*20
50092   dim undo_fgcolor$(1)*6, undo_bgcolor$(1)*6, undo_justify$(1)*1, undo_attr$(1)*128, undo_multiselect(1), undo_gridlines(1)
50094   dim undo_protected(1), undo_invisible(1), undo_tooltip$(1)*255,undo_cnvrtin$(1)*255,undo_cnvrtout$(1)*255, undo_userdata$(1)*255
50096 ! 
50098   def fnpreserveundo(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
50100     mat undo_screenio$(udim(mat screenio$)) = screenio$
50102     mat undo_screenio(udim(mat screenio)) = screenio
50104     mat undo_controlname$(udim(mat controlname$)) = controlname$
50106     mat undo_fieldname$(udim(mat fieldname$)) = fieldname$
50108     mat undo_description$(udim(mat description$)) = description$
50110     mat undo_vposition(udim(mat vposition)) = vposition
50112     mat undo_hposition(udim(mat hposition)) = hposition
50114     mat undo_fieldtype$(udim(mat fieldtype$)) = fieldtype$
50116     mat undo_specwidth(udim(mat specwidth)) = specwidth
50118     mat undo_width(udim(mat width)) = width
50120     mat undo_height(udim(mat height)) = height
50122     mat undo_truevalue$(udim(mat truevalue$)) = truevalue$
50124     mat undo_falsevalue$(udim(mat falsevalue$)) = falsevalue$
50126     mat undo_function$(udim(mat function$)) = function$
50128     mat undo_picture$(udim(mat picture$)) = picture$
50130     mat undo_parent$(udim(mat parent$)) = parent$
50132     mat undo_fgcolor$(udim(mat fgcolor$)) = fgcolor$
50134     mat undo_bgcolor$(udim(mat bgcolor$)) = bgcolor$
50136     mat undo_justify$(udim(mat justify$)) = justify$
50138     mat undo_attr$(udim(mat attr$)) = attr$
50140     mat undo_protected(udim(mat protected)) = protected
50142     mat undo_invisible(udim(mat invisible)) = invisible
50144     mat undo_tooltip$(udim(mat tooltip$)) = tooltip$
50146     mat undo_cnvrtin$(udim(mat cnvrtin$)) = cnvrtin$
50148     mat undo_cnvrtout$(udim(mat cnvrtout$)) = cnvrtout$
50150     mat undo_multiselect(udim(mat multiselect)) = multiselect
50152     mat undo_gridlines(udim(mat gridlines)) = gridlines
50154     mat undo_userdata$(udim(mat userdata$)) = userdata$
50156   fnend 
50158 ! 
50160   def fncheckundo(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,failed)
50162     let failed=failed or (~fnsameas(mat screenio$,mat undo_screenio$))
50164     let failed=failed or (~fnsamea(mat screenio,mat undo_screenio))
50166     let failed=failed or (~fnsameas(mat controlname$,mat undo_controlname$))
50168     let failed=failed or (~fnsameas(mat fieldname$,mat undo_fieldname$))
50170     let failed=failed or (~fnsameas(mat description$,mat undo_description$))
50172     let failed=failed or (~fnsamea(mat vposition,mat undo_vposition))
50174     let failed=failed or (~fnsamea(mat hposition,mat undo_hposition))
50176     let failed=failed or (~fnsameas(mat fieldtype$,mat undo_fieldtype$))
50178     let failed=failed or (~fnsamea(mat specwidth,mat undo_specwidth))
50180     let failed=failed or (~fnsamea(mat width,mat undo_width))
50182     let failed=failed or (~fnsamea(mat height,mat undo_height))
50184     let failed=failed or (~fnsameas(mat truevalue$,mat undo_truevalue$))
50186     let failed=failed or (~fnsameas(mat falsevalue$,mat undo_falsevalue$))
50188     let failed=failed or (~fnsameas(mat function$,mat undo_function$))
50190     let failed=failed or (~fnsameas(mat picture$,mat undo_picture$))
50192     let failed=failed or (~fnsameas(mat parent$,mat undo_parent$))
50194     let failed=failed or (~fnsameas(mat fgcolor$,mat undo_fgcolor$))
50196     let failed=failed or (~fnsameas(mat bgcolor$,mat undo_bgcolor$))
50198     let failed=failed or (~fnsameas(mat justify$,mat undo_justify$))
50200     let failed=failed or (~fnsameas(mat attr$,mat undo_attr$))
50202     let failed=failed or (~fnsamea(mat protected,mat undo_protected))
50204     let failed=failed or (~fnsamea(mat protected,mat undo_protected))
50206     let failed=failed or (~fnsameas(mat tooltip$,mat undo_tooltip$))
50208     let failed=failed or (~fnsameas(mat cnvrtin$,mat undo_cnvrtin$))
50210     let failed=failed or (~fnsameas(mat cnvrtout$,mat undo_cnvrtout$))
50212     let failed=failed or (~fnsamea(mat multiselect,mat undo_multiselect))
50214     let failed=failed or (~fnsamea(mat gridlines,mat undo_gridlines))
50216     let failed=failed or (~fnsameas(mat userdata$,mat undo_userdata$))
50218     let fncheckundo=(~failed)
50220   fnend 
50222 ! 
50224 ! 
60000 !  #Autonumber# 60000,2
60002 SELECTSCREEN: !  Print Screen Selection Window
60004   def fnselectscreen$*64(;selectotherfiles,___,window,selectedscreen$*64,choice,rowsub,s$*255,tempscreen$*255,helptext$*255,hl$)
60006     dim ss_caption$(3),ss_width(3),ss_spec$(3)
60008     dim ss_screencodes$(1),ss_createdates(1),ss_modifydates(1)
60010     do 
60012       let fnreadallscreencodes(mat ss_screencodes$,mat ss_createdates,mat ss_modifydates)
60014       if udim(mat ss_screencodes$) then 
60016         execute "config keyboard 04 1000" ! Remap Del As Shift F6 (Fkey 16)
60018         execute "config keyboard 0B00 0A0A0A636F6E206B657920636C6561720D" ! Shift F1 Key To "con key clear<CR>"
60020         open #(window:=fngetfilenumber): "SROW=3,SCOL=25,ROWS=25,COLS=50,Border=S,Caption=Load File",display,outin 
60022         let ss_caption$(1)="Screen Code"
60024         let ss_width(1)=30
60026         let ss_spec$(1)="C 18"
60028 !            let Ss_Spec$(1)="C 18,L"
60030         let ss_caption$(2)="Modified"
60032         let ss_caption$(3)="Created"
60034         mat ss_width(2:3)=(9)
60036         mat ss_spec$(2:3)=("DATE(m/dd/ccyy)")
60038 !            mat Ss_Spec$(2:3)=("DATE(m/dd/ccyy),L")
60040 ! 
60042         if fn42 then let hl$="4;" else let hl$="3;"
60044 ! 
60046         if udim(mat ss_screencodes$)>23 then let ss_width(1)-=1
60048         let helptext$=hl$&"Accept the selection;"&hl$&"Cancel without selecting anything.;"
60050         print #window, fields "25,29,CC 9,/W:W,B37;25,40,CC 10,/W:W,B99", help helptext$ : "Ok","Cancel"
60052         let helptext$=hl$&"Launch this screen in a new editor window;"
60054         print #window, fields "25,2,13/CC 15,/W:W,B42", help helptext$ : "Launch Screen"
60056         if selectotherfiles then 
60058           let helptext$=hl$&"Select a program or proc file to run instead of running a screen directly.;"
60060           print #window, fields "25,17,10/CC 12,/W:W,B43", help helptext$ : "Select Pgm"
60062         end if 
60064         print #window, fields "1,1,LIST 23/50,HEADERS" : (mat ss_caption$, mat ss_width, mat ss_spec$)
60066         print #window, fields "1,1,LIST 23/50,=" : (mat ss_screencodes$,mat ss_modifydates,mat ss_createdates)
60068         print #window, fields "1,1,LIST 23/50,SORT" : 1
60070 ! 
60072         do 
60074 !               do
60076           input #window, fields "24,1,SEARCH 50,,1,1;1,1,LIST 23/50,ROWSUB,SELONE,37" : s$,rowsub
60078 !               loop while fkey=105 or fkey=106
60080           if rowsub then let selectedscreen$=ss_screencodes$(rowsub)
60082           if fkey=16 and trim$(selectedscreen$)<>"" then 
60084             let choice=msgbox("Are you sure you want to completly delete "&trim$(selectedscreen$)&" ?","Delete","yN","QST")
60086             if choice=2 then ! 2 Is Yes
60088               let fndeletescreen(trim$(selectedscreen$))
60090             end if 
60092           end if 
60094           if fkey=42 and trim$(selectedscreen$)<>"" then 
60096             let fnlaunchscreen(trim$(selectedscreen$))
60098           end if 
60100           if fkey=43 then 
60102             let tempscreen$=fnselectfile$
60104             if tempscreen$><"" then 
60106               let selectedscreen$="%"&trim$(tempscreen$)(1:63)
60108             else 
60110               let fkey(-1)
60112             end if 
60114           end if 
60116         loop until fkey=99 or fkey=93 or fkey=37 or fkey=43 or fkey=0 or fkey=201 or (fkey=16 and choice=2)
60118         if fkey=37 or fkey=0 or fkey=201 or fkey=43 then 
60120           let fnselectscreen$=selectedscreen$
60122         end if 
60124         close #window: 
60126         execute "config keyboard clear" ! Reset Keyboard After Routine Exits
60128       else 
60130         let msgbox("There are no saved screens to load.","Load","ok","INF")
60132       end if 
60134     loop until fkey=99 or fkey=93 or fkey=37 or fkey=43 or fkey=201 or fkey=0 or udim(mat ss_screencodes$)=0
60136   fnend 
60138 ! 
60140   def fnselectfile$*64(;___,selectfile)
60142     open #(selectfile:=fngetfilenumber): "name=open:\*.*, recl=1023",external,input error ignore
60144     if file(selectfile)>-1 then 
60146       let fnselectfile$=file$(selectfile)(1:64)
60148       close #selectfile: 
60150     end if 
60152   fnend 
60154 ! 
60156 SELECTLAYOUT: !  Print Screen Selection Window
60158   def fnselectlayout$(current$;&function,___,window,selectedlayout$,x$)
60160     dim sl_caption$(1),sl_width(1),sl_spec$(1)
60162     dim sl_layouts$(1)
60164     let fnreadlayouts(mat sl_layouts$)
60166     if udim(mat sl_layouts$) then 
60168       open #(window:=fngetfilenumber): "SROW=4,SCOL=35,ROWS=24,COLS=30,Border=S,Caption=Select Layout",display,outin 
60170       let sl_caption$(1)="File Layout"
60172       let sl_width(1)=30
60174       let sl_spec$(1)="CC 18"
60176       if udim(mat sl_layouts$)>23 then let ss_width(1)-=1
60178       print #window, fields "24,2,CC 8,/W:W,B37;24,11,CC 8,/W:W,B40;24,20,CC 8,/W:W,B99" : "Ok","Clear","Cancel"
60180       print #window, fields "1,1,LIST 22/30,HEADERS,/W:W" : (mat sl_caption$, mat sl_width, mat sl_spec$)
60182       print #window, fields "1,1,LIST 22/30,=" : (mat sl_layouts$)
60184       print #window, fields "1,1,LIST 22/30,SORT" : 1
60186       do 
60188         input #window, fields "23,1,30/SEARCH 18,/W:W,1,1;1,1,LIST 22/30,ROW,SELONE,37" : x$, selectedlayout$
60190       loop until fkey=99 or fkey=37 or fkey=0 or fkey=201 or fkey=40 or ((fkey>1100) and (fkey<1699)) or (fkey=98) or (fkey=93) or (fkey=44) or (fkey=19)
60192       if fkey = 37 or fkey = 0 or fkey = 201 then ! #Select# Fkey #Case# 37 # 0 # 201
60194         let fnselectlayout$=selectedlayout$
60196       else if fkey = 40 then ! #Case# 40
60198         let fnselectlayout$=""
60200       else ! #Case Else#
60202         let fnselectlayout$=current$
60204       end if  ! #End Select#
60206       close #window: 
60208       if ((fkey>1100) and (fkey<1699)) or (fkey=98) or (fkey=93) or (fkey=44) or (fkey=19) then 
60210         let function=fkey
60212       end if 
60214     else 
60216       let msgbox("There are no file layouts found.","Select","ok","INF")
60218     end if 
60220 ! 
60222   fnend 
60224 ! 
60226   dim se_capspec$(1)*40
60228   dim se_descr$(1)*40
60230   dim se_butspec$(1)*40
60232   dim se_buttext$(1)
60234   dim se_inspec$(1)*30
60236   dim se_inttip$(1)*255
60238 ! 
60240 ! 
60242   dim se_ssubs$(1)*25
60244   dim se_nsubs$(1)*25
60246   dim se_sspec$(1)*20
60248   dim se_nspec$(1)*20
60250   dim se_sdescr$(1)*40
60252   dim se_ndescr$(1)*40
60254   dim se_prefix$
60256 ! 
60258 SELECTEVENTS: ! Opens A Child Window To Select Event Handler Functions
60260   def fnselectevents(mat screenio$, mat screenio;&function,___,window,eventsub)
60262 ! 
60264     if (udim(mat se_capspec$)<>(1+si_exitfn-si_enterfn)) then 
60266 ! .      ! Initialize Events Window
60268       let fnreadlayoutarrays("screenio",se_prefix$,mat se_ssubs$,mat se_nsubs$,mat se_sspec$,mat se_nspec$,mat se_sdescr$,mat se_ndescr$)
60270       mat se_capspec$(1+si_exitfn-si_enterfn)
60272       mat se_descr$(1+si_exitfn-si_enterfn)
60274       mat se_butspec$(1+si_exitfn-si_enterfn)
60276       mat se_buttext$(1+si_exitfn-si_enterfn)
60278       mat se_inspec$(1+si_exitfn-si_enterfn)
60280       mat se_inttip$(1+si_exitfn-si_enterfn)
60282 ! 
60284       for eventsub=si_enterfn to si_exitfn
60286         let se_capspec$(1+eventsub-si_enterfn)=str$(1+eventsub-si_enterfn)&",1,20/CR 25"
60288         let se_descr$(1+eventsub-si_enterfn)=se_sdescr$(eventsub)(1:24)&":"
60290         let se_butspec$(1+eventsub-si_enterfn)=str$(1+eventsub-si_enterfn)&",56,CC 4,/W:W,B"&str$(31+eventsub-si_enterfn)
60292         let se_buttext$(1+eventsub-si_enterfn)="Edit"
60294         let se_inspec$(1+eventsub-si_enterfn)=str$(1+eventsub-si_enterfn)&",22,32/V 255,/W:#FFFF77"
60296         let se_inttip$(1+eventsub-si_enterfn)=trim$(se_sdescr$(eventsub))&" event function. The Screen Level Event functions run when various Screen Level Events are triggered. See the documentation at http://www.brwiki.com keyword ""ScreenIO"" for more details."
60298       next eventsub
60300     end if 
60302 ! 
60304     let fnfixtooltips(mat se_inttip$)
60306 ! 
60308     open #(window:=fngetfilenumber): "SROW=10,SCOL=26,ROWS="&str$(1+si_exitfn-si_enterfn)&",COLS=60,Border=S,Caption=Configure Event Handlers",display,outin 
60310     print #window, fields mat se_capspec$ : mat se_descr$
60312     print #window, fields mat se_butspec$ : mat se_buttext$
60314 ! 
60316     do 
60318       rinput #window, fields mat se_inspec$, help mat se_inttip$ : mat screenio$(si_enterfn:si_exitfn)
60320       let function=fkey
60322 ! 
60324       if function>30 and function<(32+si_exitfn-si_enterfn) then 
60326         let eventsub=si_enterfn+fkey-31
60328         if len(trim$(screenio$(eventsub))) and exists(fncustomfilenameof$(screenio$(eventsub))) then 
60330           let fneditcustomfunction(screenio$(eventsub))
60332         else 
60334           let screenio$(eventsub)=fnselectfunctiondialog$(screenio$(eventsub),function,1000+eventsub)
60336         end if 
60338       end if 
60340     loop until function=99 or function=98 or function=93 or ((function>1100) and (function<1699)) or function=44 or function=19
60342 ! 
60344     if window and file(window)>-1 then close #window: 
60346 ! 
60348   fnend 
60350 ! 
60352   dim debugoutttip$(1)*255
60354   dim debuginttip$(5)*255
60356 ! 
60358 CONFIGUREDEBUG: ! Opens A Child Window To Configure The Debug Command
60360   def fnconfiguredebug(mat screenio$, mat screenio;&function,___,window,routinename$*64,newscreen$*64,record$)
60362     open #(window:=fngetfilenumber): "SROW=11,SCOL=31,ROWS=6,COLS=50,Border=S,Caption=Configure Debug Command",display,outin 
60364     print #window, fields "1,1,16/CR 20;2,1,16/CR 20;3,1,16/CR 20;4,1,16/CR 20;5,1,16/CR 20;6,1,16/CR 20" : "Launch Screen:","Key$:","ParentKey$:","Record:","Path:","mat PassedData:"
60366 ! 
60368     let debugoutttip$(1)="Select the screen to run when testing this screen."
60370     let debuginttip$(1)="Enter the value to pass in for Key$ when testing."
60372     let debuginttip$(2)="Enter the value to pass in for ParentKey$ when testing."
60374     let debuginttip$(3)="Enter the value to pass in for Record when testing."
60376     let debuginttip$(4)="Enter the optional alternate data path to use when testing. See the fileio documentation for more details on this parameter. If you don't know what its for, you don't need it."
60378     let debuginttip$(5)="Enter the values to use for mat PassedData$ when testing, seperated by commas."
60380 ! 
60382     let fnfixtooltips(mat debugoutttip$)
60384     let fnfixtooltips(mat debuginttip$)
60386 ! 
60388     do 
60390       if trim$(screenio$(si_debugscreen))="" then 
60392         let routinename$="Self"
60394       else 
60396         let routinename$=trim$(screenio$(si_debugscreen))
60398       end if 
60400 ! 
60402       let record$=str$(screenio(si_debugrecord))
60404 ! 
60406       print #window, fields "1,20,17/CC 64,/W:W,B31", help mat debugoutttip$ : routinename$
60408       rinput #window, fields "2,18,21/V 255,/W:#FFFF77;3,18,21/V 255,/W:#FFFF77;4,18,21/V 18,/W:#FFFF77;5,18,21/V 255,/W:#FFFF77;6,18,21/V 255,/W:#FFFF77", help mat debuginttip$ : screenio$(si_debugkey),screenio$(si_debugparentkey),record$,screenio$(si_debugpath), screenio$(si_debugpassed)
60410       let screenio(si_debugrecord)=val(record$) conv ignore
60412       let function=fkey
60414 ! 
60416       if function=31 then 
60418 ! .         !Select the screen to use
60420         let newscreen$=fnselectscreen$(1)
60422         if newscreen$<>"" then 
60424           if lwrc$(trim$(newscreen$))=lwrc$(trim$(screenio$(si_screencode))) then 
60426             let routinename$="Self"
60428             let screenio$(si_debugscreen)=""
60430           else 
60432             let routinename$=screenio$(si_debugscreen)=trim$(newscreen$)
60434           end if 
60436         end if 
60438       end if 
60440     loop until function=99 or function=98 or function=93 or ((function>1100) and (function<1699)) or function=44 or function=19
60442 ! 
60444     close #window: 
60446 ! 
60448   fnend 
60450 ! 
60452 CONFIGUREPROTECTED: ! Configure The Appearance of Protected Controls
60454   def fnconfigureprotected(mat screenio$, mat screenio;&function,___,window)
60456     open #(window:=fngetfilenumber): "SROW=11,SCOL=36,ROWS=10,COLS=40,Border=S,Caption=Configure Protected Style Attribute",display,outin 
60458     print #window, fields "1,1,17/CR 20;2,1,17/CR 20;3,1,17/CR 20;5,1,17/CR 20;6,1,17/CR 20;7,1,17/CR 20;9,1,17/CR 20;10,1,17/CR 20" : "Protected Text:","Protected Checkbox:","Protected Button:","Other Changes Color:","My Changes Color:","Lock Message Color:","Active Color:","UserData:"
60460 ! 
60462     do 
60464       rinput #window, fields "1,19,21/V 255,/W:#FFFF77;2,19,21/V 255,/W:#FFFF77;3,19,21/V 255,/W:#FFFF77;5,19,21/V 255,/W:#FFFF77;6,19,21/V 255,/W:#FFFF77;7,19,21/V 255,/W:#FFFF77;9,19,21/V 255,/W:#FFFF77;10,19,21/V 255,/W:#FFFF77" : mat screenio$(si_protectedtext:si_protectedbutton),mat screenio$(si_otherchanges:si_activecolor),screenio$(si_userdata)
60466       let function=fkey
60468     loop until function=99 or function=98 or function=93 or ((function>1100) and (function<1699)) or function=44 or function=19
60470     close #window: 
60472   fnend 
60474 ! 
60476 ! 
60478 TRYTOCLOSE: !  Function Closes A Window If It Was Open
60480   def fntrytoclose(windownumber)
60482     if windownumber then 
60484       if file(windownumber)<>-1 then 
60486         close #windownumber: 
60488       end if 
60490       let windownumber=0
60492     end if 
60494   fnend 
60496 ! 
60498 FILETOSCREEN: !  Converts Given File To String Based On Passed Subscripts
60500   def fnfiletoscreen(mat f$,mat f,mat screen$,mat subs;___,_i)
60502     for _i=1 to udim(mat subs)
60504       if subs(_i) >0 then 
60506         let screen$(_i)=f$(subs(_i))
60508       else if subs(_i) < 0 then 
60510         if abs(subs(_i))=si_border or abs(subs(_i))=si_screeniolocking or abs(subs(_i))=si_screeniomerge then 
60512           if f(abs(subs(_i))) then 
60514             let screen$(_i)="^"
60516           else 
60518             let screen$(_i)=""
60520           end if 
60522         else 
60524           let screen$(_i)=str$(f(abs(subs(_i))))
60526         end if 
60528       end if 
60530     next _i
60532   fnend 
60534 ! 
60536 SCREENTOFILE: !  Converts Given File To String Based On Passed Subscripts
60538   def fnscreentofile(mat f$,mat f,mat screen$,mat subs;___,_i)
60540     for _i=1 to udim(mat subs)
60542       if subs(_i) >0 then 
60544         let f$(subs(_i))=screen$(_i)
60546       else if subs(_i) < 0 then 
60548         if abs(subs(_i))=si_border or abs(subs(_i))=si_screeniolocking or abs(subs(_i))=si_screeniomerge then 
60550           if screen$(_i)(1:1)="^" then 
60552             let f(abs(subs(_i)))=1
60554           else 
60556             let f(abs(subs(_i)))=0
60558           end if 
60560         else 
60562           let f(abs(subs(_i)))=val(screen$(_i)) conv ignore
60564         end if 
60566       end if 
60568     next _i
60570   fnend 
60572 ! 
60574 SAMEAS: !   ***** Compares Two String Arrays Returning True If Same
60576   def fnsameas(mat a$,mat b$;___,failed,index)
60578     if udim(mat a$)=udim(mat b$) then 
60580       for index=1 to udim(mat a$)
60582         if trim$(a$(index))<>trim$(b$(index)) then let failed=1
60584       next index
60586       let fnsameas=(~(failed))
60588     end if 
60590   fnend 
60592 ! 
60594 SAMEA: !   ***** Compares  Two Number Arrays Returning True If Same
60596   def fnsamea(mat a,mat b;___,failed,index)
60598     if udim(mat a)=udim(mat b) then 
60600       for index=1 to udim(mat a)
60602         if a(index)<>b(index) then let failed=1
60604       next index
60606       let fnsamea=(~(failed))
60608     end if 
60610   fnend 
60612 ! 
60614 SEARCHCLOSELY: !   ***** Searches Mat A$ Looking For A Partial Match To B$ And Optionally C$ (Case Insensitive)
60616   def fnsearchclosely(mat a$,b$;c$,___,index,found)
60618     for index=1 to udim(mat a$)
60620       if trim$(c$)="" then 
60622         if pos(lwrc$(a$(index)),lwrc$(b$)) then let found=index
60624       else 
60626         if pos(lwrc$(a$(index)),lwrc$(b$)) and pos(lwrc$(a$(index)),lwrc$(c$)) then let found=index
60628       end if 
60630     next index
60632     let fnsearchclosely=found
60634   fnend 
60636 ! 
60638 TESTATTRIBUTES: !  Tests The User Attributes
60640   def fntestattributes(attributes$*255,vpos,hpos;___,weditor)
60642     let fnchangeforcevisibility(0)
60644     open #(weditor:=fngetfilenumber): "SROW=" & str$(vpos) & ", SCOL=" & str$(hpos) & ", ROWS=1, COLS=1, " & screenio$(si_attributes), display, outin error ignore
60646     if file(weditor)=-1 then 
60648       let fntestattributes=0
60650     else 
60652       let fntestattributes=1
60654       close #weditor: 
60656     end if 
60658   fnend 
60660 ! 
60662   dim preservenongui$*10000
60664   dim preservenonguirows
60666   dim preservenonguicols
60668 ! 
60670 PRESERVENONGUI: ! Preserve the Non Gui Window
60672   def fnpreservenongui
60674     let fnreadscreensize(preservenonguirows,preservenonguicols,0)
60676     input #0, fields "1,1,C "&str$(preservenonguirows*preservenonguicols)&",G" : preservenongui$
60678     execute "config gui on"
60680   fnend 
60682 ! 
60684   def fnrestorenongui
60686     execute "config gui off"
60688     open #0: "rows="&str$(preservenonguirows)&", cols="&str$(preservenonguicols),display,outin 
60690     print #0, fields "1,1,C "&str$(preservenonguirows*preservenonguicols) : preservenongui$
60692   fnend 
60694 ! 
60696 ! 
60698   dim screensize(4), fontsize(2)
60700 READSCREENSIZE: ! Return The Screen Size Of Window 0 In Terms Of Rows And Columns
60702   def fnreadscreensize(&rows,&cols;parentwindow)
60704     if ~fn42e then 
60706       let fnoldreadscreensize(rows,cols,parentwindow)
60708     else 
60710       let file(parentwindow,"USABLE_RECT",mat screensize)
60712       let file(parentwindow,"FONTSIZE",mat fontsize)
60714       let rows=(screensize(4)/fontsize(1))
60716       let cols=(screensize(3)/fontsize(2))
60718     end if 
60720   fnend 
60722 ! 
60724   def fnoldreadscreensize(&rows,&cols;parentwindow,___,ffile,string$*2000,srow,scol,erow,ecol,position)
60726     if env$("guimode")<>"ON" then 
60728       let rows=val(env$("screensize_rows")) conv ignore
60730       let cols=val(env$("screensize_cols")) conv ignore
60732       if ~rows then let rows=24
60734       if ~cols then let cols=80
60736     else 
60738       execute "Status Files >[SESSION].tmp"
60740       open #(ffile:=fngetfilenumber) : "name=[SESSION].tmp",display,input 
60742 ! 
60744       do while file(ffile)=0
60746         linput #ffile: string$ eof ignore
60748       loop until string$(1:5)="-----"
60750       do while file(ffile)=0
60752         linput #ffile: string$ eof ignore
60754       loop until (lwrc$(string$)(1:4)="open") and trim$(string$(pos(string$,"#")+1:pos(string$,"#")+5))=str$(parentwindow)
60756       if file(ffile)=0 then 
60758         linput #ffile: string$ eof ignore
60760       end if 
60762 ! 
60764       close #ffile: 
60766       execute "Free [SESSION].tmp" error RETRYFIVETIMES
60768 ! 
60770       if (position:=pos(uprc$(string$),"SROW=")) then 
60772         let srow=val(string$(position+5:pos(string$,",",position)-1)) conv ignore
60774       end if 
60776       if (position:=pos(uprc$(string$),"SCOL=")) then 
60778         let scol=val(string$(position+5:pos(string$,",",position)-1)) conv ignore
60780       end if 
60782       if (position:=pos(uprc$(string$),"EROW=")) then 
60784         let erow=val(string$(position+5:pos(string$,",",position)-1)) conv ignore
60786       end if 
60788       if (position:=pos(uprc$(string$),"ECOL=")) then 
60790         let ecol=val(string$(position+5:pos(string$,",",position)-1)) conv ignore
60792       end if 
60794       if srow and erow then let rows=erow-srow+1
60796       if scol and ecol then let cols=ecol-scol+1
60798     end if 
60800   fnend 
60802 ! 
60804 !   $$$$$ - figure out why this doesnt work at Luis's location. Or not, we don't support Client Server anyway.
60806 !   def Fnclientserver(;___,Serverfolder$*255,Clientfolder$*255)
60808 !      let Serverfolder$=Os_Filename$(".")
60810 !      let Clientfolder$=Os_Filename$("@:.")
60812 !      if Clientfolder$(Len(Clientfolder$):Len(Clientfolder$))<>"\" then
60814 !         let Clientfolder$=Clientfolder$&"\"
60816 !      end if
60818 !      if Serverfolder$(Len(Serverfolder$):Len(Serverfolder$))<>"\" then
60820 !         let Serverfolder$=Serverfolder$&"\"
60822 !      end if
60824 !      if Lwrc$(Trim$(Serverfolder$))=Lwrc$(Trim$(Clientfolder$)) then
60826 !         let Fnclientserver=0
60828 !      else 
60830 !         let Fnclientserver=1
60832 !      end if
60834 !   fnend
60836 ! 
60838 ! **********************************************************************
60840 ! Functions To Get Info From Config.Sys Files. By Susan Smith.
60842 !  Updated By Gabriel Bakker - To Save Time By Running Only Once.
60844 ! 
60846   dim configfile$*300
60848   dim executablefile$*300
60850 ! 
60852   def fngetwbcfg$*255
60854     if configfile$="" then 
60856       let fnreadconfigandexe
60858     end if 
60860     let fngetwbcfg$=configfile$
60862   fnend 
60864 ! 
60866   def fngetbrexe$*255
60868     if executablefile$="" then 
60870       let fnreadconfigandexe
60872     end if 
60874     let fngetbrexe$=executablefile$
60876   fnend 
60878 ! 
60880   dim configstat$*30000
60882   def fnreadconfigandexe(;___,cr$,filenumber,qstart,qend)
60884     let cr$=hex$("0D0A")
60886     execute "status files >TEMPFILE.[session]"
60888     if exists("TEMPFILE.[session]") then 
60890       open #(filenumber:=fngetfilenumber): "name=TEMPFILE.[session],eol=none",display,input error ignore
60892       if ~file(filenumber) then 
60894         linput #filenumber: configstat$
60896         close #filenumber: 
60898 ! 
60900         let qstart=pos(uprc$(configstat$),"CONFIG FILE")
60902         if qstart>0 then 
60904           let configfile$=configstat$(qstart+13:qstart+300)
60906           let qend=pos(configfile$,cr$)
60908           let configfile$=configfile$(1:qend-1)
60910           if configfile$(1:1)=":" then let configfile$=os_filename$(configfile$)
60912         end if 
60914 ! 
60916         let qstart=pos(uprc$(configstat$),"EXECUTABLE FILE")
60918         if qstart>0 then 
60920           let executablefile$=configstat$(qstart+17:qstart+300)
60922           let qend=pos(executablefile$,cr$)
60924           let executablefile$=executablefile$(1:qend-1)
60926           if executablefile$(1:1)=":" then let executablefile$=os_filename$(executablefile$)
60928         end if 
60930       end if 
60932     end if 
60934     if exists("c:\TEMPFILE.[session]") then execute "free TEMPFILE.[session] -N"
60936   fnend 
60938 ! 
60940   dim gs_data$(7)
60942   dim gs_field$(1)*64,gs_fieldspec$(1)*32,gs_fielddesc$(1)*255
60944 ! 
60946   def fngeneratescreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,generate,newfield,index,listviewfield,rightedge,titlefield,stophere,openotherwindow,captionindex,cancelbutton,savebutton,addbutton,editbutton,deletebutton)
60948 ! 
60950     let gs_layout=1
60952     let gs_buildlistview=2
60954     let gs_namelistview=3
60956     let gs_buildaddedit=4
60958     let gs_nameaddedit=5
60960     let gs_buildcombo=6
60962     let gs_namecombo=7
60964 ! 
60966 ! .   ! Set Input Defaults
60968     let gs_data$(gs_buildlistview)=gs_data$(gs_buildaddedit)="^"
60970     if len(trim$(screenio$(si_filelay))) then 
60972       let gs_data$(gs_layout)=gs_lastchoice$=trim$(screenio$(si_filelay))
60974       let gs_data$(gs_namelistview)=uprc$(trim$(screenio$(si_filelay)))(1:14)&"LIST"
60976       let gs_data$(gs_nameaddedit)=uprc$(trim$(screenio$(si_filelay)))(1:14)&"EDIT"
60978       let gs_data$(gs_namecombo)=uprc$(trim$(screenio$(si_filelay)))(1:13)&"COMBO"
60980     end if 
60982 ! 
60984 ! 
60986     let generate=fninputmainpage(mat gs_data$)
60988 ! 
60990     if generate then 
60992 ! .      ! Put the generate screen code here.
60994       if gs_data$(gs_buildlistview)(1:1)="^" then ! Create Listview Screen
60996         let generate=fninputfields(gs_data$(gs_layout),"Select fields for Listview",mat gs_field$,mat gs_fieldspec$,mat gs_fielddesc$)
60998 ! 
61000         if generate then 
61002           let fnreadscreen("",mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) ! Init Blank Screen
61004           let fngenerateheader(gs_data$(gs_namelistview),gs_data$(gs_layout),mat screenio$,mat screenio)
61006 ! 
61008           mat gaps=(0)
61010           let titlefield=fnaddatitle(gs_data$(gs_layout),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat screenio$,mat screenio)
61012           let fnskipagap
61014           let fnskipagap
61016 ! 
61018 ! .            ! Add a listview, make it big
61020           let listviewfield=fnaddlistview(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
61022           let hposition(listviewfield)=10
61024           let width(listviewfield)=screenio(si_hsize)-18
61026           let height(listviewfield)=screenio(si_vsize)-8
61028           let fgcolor$(listviewfield)="W"
61030           let bgcolor$(listviewfield)="W"
61032 ! 
61034 ! .            ! Add some columns
61036           for index=1 to udim(mat gs_field$)
61038             let newfield=fnaddcurrentlistchld(parent$(listviewfield),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,gs_field$(index),gs_fielddesc$(index),gs_fieldspec$(index))
61040           next index
61042 ! 
61044 ! .            ! Add a Search box
61046           let newfield=fnaddsearchbox(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
61048           let specwidth(listviewfield)=1 ! Set it to search 1st column
61050           let fgcolor$(newfield)="W"
61052           let bgcolor$(newfield)="W"
61054 ! 
61056 ! .            ! Add Exit Buttons
61058           let fnaddexitbuttons(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
61060 ! 
61062 ! .            ! If there's also an add/edit screen then
61064           if gs_data$(gs_buildaddedit)(1:1)="^" then ! Create Add/Edit Screen
61066 ! .               ! Add Add/Edit/Delete buttons
61068             let fnaddaebuttons(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,gs_data$(gs_nameaddedit))
61070           end if 
61072 ! 
61074 ! .            ! Save the screen
61076           if fnwritescreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
61078             if gs_data$(gs_buildaddedit)(1:1)="^" then ! Create Add/Edit Screen
61080               let openotherwindow=1
61082             end if 
61084           else 
61086             let msgbox("Error saving listview screen.")
61088             let stophere=1
61090           end if 
61092         end if 
61094       end if 
61096 ! 
61098       if ~stophere then 
61100         if gs_data$(gs_buildaddedit)(1:1)="^" then ! Create Add/Edit Screen
61102           let generate=fninputfields(gs_data$(gs_layout),"Select fields for Add/Edit",mat gs_field$,mat gs_fieldspec$,mat gs_fielddesc$)
61104           if generate then 
61106 ! 
61108             let fnreadscreen("",mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) ! Init Blank Screen
61110             let fngenerateheader(gs_data$(gs_nameaddedit),gs_data$(gs_layout),mat screenio$,mat screenio,1)
61112             mat gaps=(0)
61114             let titlefield=fnaddatitle(gs_data$(gs_layout),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat screenio$,mat screenio,1)
61116             let fnskipagap
61118 ! 
61120 ! .               ! Add a bunch of edit fields
61122             for index=1 to udim(mat gs_field$)
61124               let newfield=fnaddandcolorcurrentfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat screenio$,mat screenio,gs_field$(index),gs_fielddesc$(index),gs_fieldspec$(index))
61126             next index
61128 ! 
61130 ! .               ! Shrink title to match width of widest control
61132             let rightedge=0
61134             for index=1 to udim(mat hposition)
61136               if index><titlefield then 
61138                 let rightedge=max(rightedge,hposition(index)+width(index)-1)
61140               end if 
61142             next index
61144             let width(titlefield)=rightedge-8
61146 ! 
61148 ! .               ! Resize Screen
61150             let fnresizescreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
61152 ! 
61154 ! .               ! Add Exit Buttons
61156             let fnaddexitbuttons(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
61158 ! 
61160 ! .               ! Save the screen
61162             if ~fnwritescreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
61164               let msgbox("Error saving Add/Edit Screen.")
61166               let stophere=1
61168             end if 
61170           end if 
61172           if openotherwindow then ! Launch another session of BR to load the session
61174             let fnlaunchscreen(gs_data$(gs_namelistview))
61176           end if 
61178           let openotherwindow=1
61180         end if 
61182       end if 
61184 ! 
61186       if ~stophere then 
61188         if gs_data$(gs_buildcombo)(1:1)="^" then ! Create Combo Screen
61190           let generate=fninputfields(gs_data$(gs_layout),"Select fields for Combo Screen Listview",mat gs_field$,mat gs_fieldspec$,mat gs_fielddesc$)
61192           if generate then 
61194 ! 
61196             let fnreadscreen("",mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) ! Init Blank Screen
61198             let fngenerateheader(gs_data$(gs_namecombo),gs_data$(gs_layout),mat screenio$,mat screenio,1)
61200             mat gaps=(0)
61202             let titlefield=fnaddatitle(gs_data$(gs_layout),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat screenio$,mat screenio,1)
61204             let fnskipagap
61206 ! 
61208 ! .               ! Add a listview on the left
61210             let listviewfield=fnaddlistview(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
61212             let hposition(listviewfield)=5
61214             let width(listviewfield)=max(int((screenio(si_hsize)-10)/2),10)
61216             let height(listviewfield)=screenio(si_vsize)-8
61218             let fgcolor$(listviewfield)="W"
61220             let bgcolor$(listviewfield)="W"
61222 ! 
61224 ! .               ! Add some columns
61226             for index=1 to udim(mat gs_field$)
61228               let newfield=fnaddcurrentlistchld(parent$(listviewfield),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,gs_field$(index),gs_fielddesc$(index),gs_fieldspec$(index))
61230             next index
61232 ! 
61234 ! .               ! Add a Search box
61236             let newfield=fnaddsearchbox(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
61238             let specwidth(listviewfield)=1 ! Set it to search 1st column
61240             let fgcolor$(newfield)="W"
61242             let bgcolor$(newfield)="W"
61244 ! 
61246             let generate=fninputfields(gs_data$(gs_layout),"Select fields for Combo Screen Edit",mat gs_field$,mat gs_fieldspec$,mat gs_fielddesc$)
61248             if generate then 
61250               mat gaps=(0)
61252               let fnskipagap
61254               let fnskipagap
61256 ! .                  ! Add a bunch of edit fields (on the right half)
61258               for index=1 to udim(mat gs_field$)
61260                 let newfield=fnaddandcolorcurrentfield(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat screenio$,mat screenio,gs_field$(index),gs_fielddesc$(index),gs_fieldspec$(index),captionindex,index+2)
61262                 let hposition(captionindex)=hposition(listviewfield)+width(listviewfield)+5
61264                 let width(captionindex)=10
61266 ! 
61268                 let hposition(newfield)=hposition(captionindex)+width(captionindex)+2
61270                 let width(newfield)=min(max(screenio(si_hsize)-hposition(newfield)-5,5),width(newfield))
61272                 let function$(newfield)="{listviewcombovalfld}"
61274                 if index=1 then let controlname$(newfield)="FirstField"
61276               next index
61278 ! 
61280 ! .                  ! Assign a read function and mainloop to make it ignore user changes when listview sel changes too.
61282               let screenio$(si_readfn)="{listviewcomboread}"
61284               let screenio$(si_loopfn)="{listviewcombomain}"
61286 ! 
61288 ! .                  ! Assign a listview postpopulate function that calls read
61290 ! .                  ! let ScreenIO$(si_PostListviewFn)="{listviewcombopost}"
61292 ! 
61294 ! .                  ! Add Exit Buttons
61296               let fnaddexitbuttons(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,cancelbutton,savebutton)
61298               let function$(savebutton)="{listviewcombosave}"
61300               let description$(savebutton)="Save"
61302               let description$(cancelbutton)="Quit"
61304 ! 
61306 ! .                  ! Add Add/Delete Buttons
61308               let fnaddaebuttons(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,"nothing",vposition(savebutton),0,0,1,0,addbutton,editbutton,deletebutton)
61310               if addbutton then 
61312                 let function$(addbutton)="{listviewcomboadd}"
61314               end if 
61316               if deletebutton then 
61318                 let function$(deletebutton)="{listviewcombodelete}"
61320               end if 
61322 ! 
61324 ! .                  ! Save the screen
61326               if ~fnwritescreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
61328                 let msgbox("Error saving Combo Screen.")
61330                 let stophere=1
61332               end if 
61334               if openotherwindow then ! Launch another session of BR to load the session
61336                 let fnlaunchscreen(gs_data$(gs_namelistview))
61338               end if 
61340             end if 
61342           end if 
61344         end if 
61346       end if 
61348 ! 
61350     end if 
61352   fnend 
61354 ! 
61356   def fnaddatitle(filedescription$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat screenio$,mat screenio;edit,___,index)
61358     let index=fnaddcaption(mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
61360     let controlname$(index)="Title"
61362     if edit then 
61364       let description$(index)="Edit "&uprc$(filedescription$(1:1))&lwrc$(filedescription$(2:18))
61366     else 
61368       let description$(index)=uprc$(filedescription$(1:1))&lwrc$(filedescription$(2:18))&" List"
61370     end if 
61372     let justify$(index)="C"
61374     let hposition(index)=6
61376     let width(index)=screenio(si_hsize)-14
61378     let specwidth(index)=len(trim$(description$(index)))
61380     let fnaddatitle=index
61382   fnend 
61384 ! 
61386   dim gs_cap$(6),gs_capspec$(6)*50,gs_spec$(7)*50,gs_combo$*50,gs_lastchoice$,gs_layouts$(1)
61388   def fninputmainpage(mat gs_data$;___,window,done,function,cfld)
61390     open #(window:=fngetfilenumber): "srow=12,scol=20,rows=9,cols=40,border=s,caption=Generate Screens",display,outin 
61392 ! .   ! Build Caption Specs
61394     let gs_capspec$(1)="2,2,CR 10" : let gs_cap$(1)="Layout: "
61396     let gs_capspec$(2)="4,2,CR 10" : let gs_cap$(2)="Listview: "
61398     let gs_capspec$(3)="5,2,CR 10" : let gs_cap$(3)="Add/Edit: "
61400     let gs_capspec$(4)="8,20,CC 8,/W:W,B5" : let gs_cap$(4)="Generate"
61402     let gs_capspec$(5)="8,30,CC 8,/W:W,B99" : let gs_cap$(5)="Cancel"
61404     let gs_capspec$(6)="6,2,CR 10" : let gs_cap$(6)="Cmb Scrn: "
61406 ! 
61408 ! .   ! Build Input Specs
61410     let gs_combo$="2,13,COMBO 18,=,SELECT"
61412     let gs_spec$(gs_layout)="2,13,COMBO 18,X/W:W,SELECT"
61414     let gs_spec$(gs_buildlistview)="4,13,CHECK 1,/W:W"
61416     let gs_spec$(gs_buildaddedit)="5,13,CHECK 1,/W:W"
61418     let gs_spec$(gs_namelistview)="4,15,C 18,/W:W"
61420     let gs_spec$(gs_nameaddedit)="5,15,C 18,/W:W"
61422     let gs_spec$(gs_buildcombo)="6,13,CHECK 1,/W:W"
61424     let gs_spec$(gs_namecombo)="6,15,C 18,/W:W"
61426 ! 
61428 ! .   ! Print Captions
61430     print #window, fields mat gs_capspec$ : mat gs_cap$
61432 ! 
61434 ! .   ! Read File Layouts
61436     let fnreadlayouts(mat gs_layouts$)
61438 ! 
61440 ! .   ! Populate Combo Box
61442     mat gs_layouts$(udim(mat gs_layouts$)+1) ! Add blank one
61444     print #window, fields gs_combo$ : mat gs_layouts$
61446 ! 
61448 ! .   ! Check for match on the combo box
61450     if srch(mat gs_layouts$,gs_data$(1))<=0 then 
61452       let gs_data$(gs_layout)=gs_lastchoice$=""
61454       let gs_data$(gs_namelistview)=""
61456       let gs_data$(gs_nameaddedit)=""
61458       let gs_data$(gs_namecombo)=""
61460     end if 
61462 ! 
61464     do until done
61466       rinput #window, fields mat gs_spec$ : mat gs_data$
61468       let function=fkey
61470       let cfld=curfld
61472       let curfld(cfld,function)
61474 ! 
61476       if gs_lastchoice$><gs_data$(gs_layout) then 
61478         let gs_data$(gs_namelistview)=uprc$(trim$(gs_data$(gs_layout)))(1:14)&"LIST"
61480         let gs_data$(gs_nameaddedit)=uprc$(trim$(gs_data$(gs_layout)))(1:14)&"EDIT"
61482         let gs_data$(gs_namecombo)=uprc$(trim$(gs_data$(gs_layout)))(1:13)&"COMBO"
61484         let gs_lastchoice$=gs_data$(gs_layout)
61486       end if 
61488 ! 
61490       let fncleanstring(gs_data$(gs_namelistview))
61492       let fncleanstring(gs_data$(gs_nameaddedit))
61494       let fncleanstring(gs_data$(gs_namecombo))
61496 ! 
61498       if function = 5 then ! #Select# Function #Case# 5
61500         if len(trim$(gs_data$(gs_layout))) and (gs_data$(gs_buildlistview)(1:1)="^" and len(trim$(gs_data$(gs_namelistview)))) or (gs_data$(gs_buildaddedit)(1:1)="^" and len(trim$(gs_data$(gs_nameaddedit)))) or (gs_data$(gs_buildcombo)(1:1)="^" and len(trim$(gs_data$(gs_namecombo)))) then 
61502           let done=1
61504         else 
61506           let msgbox("You must select a file layout, check at least 1 box above, and enter a valid screen name.","Nothing to do.")
61508         end if 
61510       else if function = 99 or function = 93 then ! #Case# 99 # 93
61512         let done=-1
61514       end if  ! #End Select#
61516     loop 
61518 ! 
61520     close #window: 
61522 ! 
61524     if done=1 then 
61526       let fninputmainpage=1
61528     end if 
61530   fnend 
61532 ! 
61534   dim if_sfields$(1)*64,if_nfields$(1)*64
61536   dim if_sfieldsspec$(1)*32,if_nfieldsspec$(1)*32
61538   dim if_sfieldsdescription$(1)*255,if_nfieldsdescription$(1)*255
61540 ! 
61542   dim rowselected(1)
61544   def fninputfields(layout$,caption$*40,mat selectedfields$,mat selectedspec$,mat selecteddescription$;___,window,done,function,cfld,prefix$,selcount,index)
61546     open #(window:=fngetfilenumber): "srow=4,scol=36,rows=26,cols=40,border=s",display,outin 
61548 ! 
61550 ! .   ! Print listview
61552     let fieldsheadings$(1)="Name"
61554     let fieldswidths(1)=9
61556     let fieldsspec$(1)="V 50"
61558     let fieldsheadings$(2)="Description"
61560     let fieldswidths(2)=14
61562     let fieldsspec$(2)="V 255"
61564     let fieldsheadings$(3)="Type"
61566     let fieldswidths(3)=5
61568     let fieldsspec$(3)="V 30"
61570     print #window, fields "3,1,LIST 22/40,HEADERS,,1300" : (mat fieldsheadings$, mat fieldswidths, mat fieldsspec$)
61572 ! 
61574 ! .   ! Print Exit Buttons
61576     print #window, fields "26,20,CC 8,/W:W,B7;26,30,CC 8,/W:W,B99" : "Generate","Cancel"
61578     print #window, fields "1,1,CC 40" : caption$
61580 ! 
61582 ! .   ! Read Fields
61584     let fnreadlayoutarrays(layout$,prefix$,mat if_sfields$,mat if_nfields$,mat if_sfieldsspec$,mat if_nfieldsspec$,mat if_sfieldsdescription$,mat if_nfieldsdescription$)
61586 ! 
61588 ! .   ! Populate Listview
61590     print #window, fields "3,1,LIST 22/40,=,1300" : (mat if_sfields$, mat if_sfieldsdescription$, mat if_sfieldsspec$)
61592     print #window, fields "3,1,LIST 22/40,+,1300" : (mat if_nfields$, mat if_nfieldsdescription$, mat if_nfieldsspec$)
61594 ! 
61596     do until done
61598 ! 
61600       rinput #window, fields "3,1,LIST 22/40,ROWCNT,SEL" : selcount
61602       mat rowselected(selcount) ! We do it the old way so its compatible with both ways
61604       rinput #window, fields "3,1,LIST 22/40,ROWSUB,SEL,NOWAIT" : mat rowselected
61606 ! 
61608       let function=fkey
61610       let cfld=curfld
61612       let curfld(cfld,function)
61614 ! 
61616       if function = 7 then ! #Select# Function #Case# 7
61618 ! 
61620 ! .         ! Return Selected Fields
61622 ! .         ! Clear the arrays first
61624         mat selectedfields$(0)
61626         mat selectedspec$(0)
61628         mat selecteddescription$(0)
61630 ! 
61632 ! .         ! Loop through selected, and build them into passback arrays
61634         for index=1 to udim(mat rowselected)
61636           if rowselected(index)<=udim(mat if_sfields$) then 
61638             mat selectedfields$(udim(mat selectedfields$)+1)
61640             mat selectedspec$(udim(mat selectedfields$))
61642             mat selecteddescription$(udim(mat selectedfields$))
61644 ! 
61646             let selectedfields$(udim(mat selectedfields$))=if_sfields$(rowselected(index))
61648             let selectedspec$(udim(mat selectedfields$))=if_sfieldsspec$(rowselected(index))
61650             let selecteddescription$(udim(mat selectedfields$))=if_sfieldsdescription$(rowselected(index))
61652           else 
61654             mat selectedfields$(udim(mat selectedfields$)+1)
61656             mat selectedspec$(udim(mat selectedfields$))
61658             mat selecteddescription$(udim(mat selectedfields$))
61660 ! 
61662             let selectedfields$(udim(mat selectedfields$))=if_nfields$(rowselected(index)-udim(if_sfields$))
61664             let selectedspec$(udim(mat selectedfields$))=if_nfieldsspec$(rowselected(index)-udim(if_sfields$))
61666             let selecteddescription$(udim(mat selectedfields$))=if_nfieldsdescription$(rowselected(index)-udim(if_sfields$))
61668           end if 
61670         next index
61672 ! 
61674         let done=1
61676       else if function = 99 or function = 93 then ! #Case# 99 # 93
61678         let done=-1
61680       end if  ! #End Select#
61682     loop 
61684 ! 
61686     close #window: 
61688 ! 
61690     if done=1 then 
61692       let fninputfields=1
61694     end if 
61696   fnend 
61698 ! 
61700 ! 
61702   def fncleanstring(&string$;___,index)
61704     do 
61706       let index+=1
61708       if index<=len(string$) then 
61710         if pos("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",uprc$(string$(index:index)))<=0 then 
61712           let string$(index:index)=""
61714           let index-=1
61716         end if 
61718       end if 
61720     loop while index<len(string$)
61722   fnend 
61724 ! 
61726   def fngenerateheader(screenname$,layout$,mat screenio$,mat screenio;addborder)
61728     let screenio$(si_screencode)=uprc$(screenname$)
61730     let screenio$(si_filelay)=layout$
61732     let screenio(si_vsize)=24
61734     let screenio(si_hsize)=80
61736     let screenio(si_createdate)=days(date$)
61738     let screenio(si_modifydate)=days(date$)
61740     let screenio(si_border)=addborder
61742   fnend 
61744 ! 
61746 ! 
61748 ! 
61750   dim generatedcode$*20000
61752   dim displaylistview$(1)*255
61754   def fngeneratecode(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,speclist$*2000,datalist$*2000,helplist$*2000,crlf$,inputhelp,outputhelp)
61756     let generatedcode$=""
61758 ! 
61760 ! .   ! Commonly Used Strings
61762     let crlf$=chr$(13)&chr$(10)
61764 ! 
61766     let screenname$=uprc$(trim$(screenio$(si_screencode))(1:1))&lwrc$(trim$(screenio$(si_screencode))(2:18))
61768     let layoutname$=uprc$(trim$(screenio$(si_filelay))(1:1))&lwrc$(trim$(screenio$(si_filelay))(2:99))
61770 ! 
61772 ! .   ! Arrays
61774     dim output_data$(1)*1000,output_spec$(1)*255, output_help$(1)*260
61776     dim input_data$(1)*1000,input_data(1),input_spec$(1)*255, input_help$(1)*260, input_old$(1)*1000
61778 ! 
61780 ! .   ! Generate Specs
61782     let fngenerateoutputspecs(0,mat output_data$,mat output_spec$,mat output_help$,mat output_s$,mat output_screensubs$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$)
61784     let fngenerateinputspecs(0,mat input_data$,mat input_data,mat input_spec$,mat input_subs,mat input_help$,mat input_old$,mat f$,mat f,mat input_fieldsssubs$,mat input_fieldsnsubs$,mat input_s$,mat input_screensubs$,input_listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$)
61786 ! 
61788     mat input_spec$(udim(mat input_spec$)-udim(mat input_data))
61790 ! 
61792     for index=1 to udim(mat output_help$)
61794       if trim$(output_help$(index))<>"" then 
61796         let outputhelp=1
61798       end if 
61800     next index
61802     for index=1 to udim(mat input_help$)
61804       if trim$(input_help$(index))<>"" then 
61806         let inputhelp=1
61808       end if 
61810     next index
61812 ! 
61814 ! .   ! Build Header
61816     let fngc(" ! "&screenname$&" - Copyright "&date$("CCYY")&" by Sage AX")
61818     let fngc(" ! This program is the "&screenname$&" screen auto-generated by ScreenIO")
61820     let fngc(" ! ")
61822     let fngc(" ! Created: "&date$(screenio(si_createdate),"MM/DD/CCYY"))
61824     let fngc(" ! Modified: "&date$(screenio(si_modifydate),"MM/DD/CCYY"))
61826     let fngc(" ! ")
61828     let fngc
61830     let fngc("    open #0: ""rows="&str$(screenio(si_vsize))&", cols="&str$(screenio(si_hsize))&""", display, outin")
61832     let fngc("    library : fnShow"&screenname$)
61834     let fngc("    let fnShow"&screenname$)
61836     let fngc
61838     let fngc("    stop")
61840     let fngc(" ! ")
61842     let fngc
61844     let fngc(" def library fnShow"&screenname$&"(;Key$,Row,Col,___,Exitmode)")
61846     let fngc
61848 ! 
61850 ! .   ! Build Variable Declarations
61852     let fngc("    ! Declare Variables")
61854     let fngc("    dim Form$(1)*255")
61856     if layoutname$<>"" then let fngc("    dim "&layoutname$&"$(1)*255,"&layoutname$&"(1)")
61858     let fngc
61860 ! 
61862     if udim(mat output_data$) then 
61864       let fngc("    dim "&screenname$&"_Output_Spec$("&str$(udim(output_data$))&")*255,"&screenname$&"_Output_Data$("&str$(udim(output_data$))&")*1000",1)
61866       if outputhelp then let fngc(","&screenname$&"_Output_Help$("&str$(udim(output_data$))&")*260",1)
61868       let fngc
61870     end if 
61872 ! 
61874     if udim(mat input_data$) then 
61876       let fngc("    dim "&screenname$&"_Input_Spec$("&str$(udim(input_data$))&")*255,"&screenname$&"_Input_Data$("&str$(udim(input_data$))&")*1000",1)
61878       if inputhelp then let fngc(","&screenname$&"_Input_Help$("&str$(udim(input_data$))&")*260",1)
61880       let fngc
61882     end if 
61884 ! 
61886     dim gc_listspec$*255,gc_caption$(1)*255,gc_width(1),gc_spec$(1)*255
61888 ! 
61890 ! .   ! Listview Variables
61892     if udim(mat input_data) then 
61894       for index=1 to udim(mat fieldtype$)
61896         if lwrc$(trim$(fieldtype$(index)))="listview" then 
61898           let fngc
61900           let fngc("   ! Define Listviews")
61902           let gc_listspec$=fncalculatelistviewheaders$(mat gc_caption$,mat gc_width,mat gc_spec$,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
61904           let fngc("   dim "&screenname$&"_"&parent$(index)&"_Captions$("&str$(udim(gc_caption$))&"), "&screenname$&"_"&parent$(index)&"_Widths("&str$(udim(gc_caption$))&"), "&screenname$&"_"&parent$(index)&"_Specs$("&str$(udim(gc_caption$))&")*255")
61906           let fngc("   dim "&screenname$&"_"&parent$(index)&"_Spec$*255")
61908           let fngc
61910           let fngc("   read "&screenname$&"_"&parent$(index)&"_Spec$")
61912           let fngc("   data """&gc_listspec$&"""")
61914           let fngc("   read mat "&screenname$&"_"&parent$(index)&"_Captions$")
61916           let fngc("   data ",1)
61918           for jndex=1 to udim(mat gc_caption$)
61920             if jndex=udim(mat gc_caption$) then 
61922               let fngc(""""&gc_caption$(jndex)&"""")
61924             else 
61926               let fngc(""""&gc_caption$(jndex)&""", ",1)
61928             end if 
61930           next jndex
61932           let fngc("   read mat "&screenname$&"_"&parent$(index)&"_Widths")
61934           let fngc("   data ",1)
61936           for jndex=1 to udim(mat gc_width)
61938             if jndex=udim(mat gc_width) then 
61940               let fngc(str$(gc_width(jndex)))
61942             else 
61944               let fngc(str$(gc_width(jndex))&", ",1)
61946             end if 
61948           next jndex
61950           let fngc("   read mat "&screenname$&"_"&parent$(index)&"_Specs$")
61952           let fngc("   data ",1)
61954           for jndex=1 to udim(mat gc_spec$)
61956             if jndex=udim(mat gc_spec$) then 
61958               let fngc(""""&gc_spec$(jndex)&"""")
61960             else 
61962               let fngc(""""&gc_spec$(jndex)&""", ",1)
61964             end if 
61966           next jndex
61968           mat displaylistview$(udim(mat displaylistview$)+1)
61970           let displaylistview$(udim(mat displaylistview$))="    print #"&screenname$&"_Window, fields "&screenname$&"_"&parent$(index)&"_Spec$ : (mat "&screenname$&"_"&parent$(index)&"_Captions$, mat "&screenname$&"_"&parent$(index)&"_Widths, mat "&screenname$&"_"&parent$(index)&"_Specs$)"
61972         end if 
61974       next index
61976     end if 
61978 ! 
61980 ! .   ! TODO: FrameWindow Variables
61982 ! 
61984     let fngc
61986 ! 
61988 ! .   ! Build Read Statements
61990     for index = 1 to udim(output_data$)
61992       let speclist$=speclist$&""""&output_spec$(index)&""""
61994       let datalist$=datalist$&""""&output_data$(index)&""""
61996       let helplist$=helplist$&""""&output_help$(index)&""""
61998       if index < udim(output_data$) then 
62000         if mod(index,4)=0 and index > 1 then 
62002           let speclist$=speclist$&crlf$&"    data "
62004           let datalist$=datalist$&crlf$&"    data "
62006           let helplist$=helplist$&crlf$&"    data "
62008         else if mod(index,2)=0 and index > 1 then 
62010           let speclist$=speclist$&crlf$&"    data "
62012           let datalist$=datalist$&","
62014           let helplist$=helplist$&","
62016         else 
62018           let speclist$=speclist$&","
62020           let datalist$=datalist$&","
62022           let helplist$=helplist$&","
62024         end if 
62026       end if 
62028     next index
62030 ! 
62032 ! .   ! Add Read Statements for Output Controls
62034     if udim(mat output_data$) then 
62036       let fngc("    ! Read Specs for Output Controls")
62038       let fngc("    read mat "&screenname$&"_Output_Spec$")
62040       let fngc("    data "&speclist$)
62042       let fngc
62044       let fngc("    read mat "&screenname$&"_Output_Data$")
62046       let fngc("    data "&datalist$)
62048       let fngc
62050       if outputhelp then 
62052         let fngc("    read mat "&screenname$&"_Output_Help$")
62054         let fngc("    data "&helplist$)
62056         let fngc
62058       end if 
62060     end if 
62062 ! 
62064     let speclist$=""
62066     let helplist$=""
62068     for index = 1 to udim(input_spec$)
62070       let speclist$=speclist$&""""&input_spec$(index)&""""
62072       let helplist$=helplist$&""""&input_help$(index)&""""
62074       if index < udim(input_data$) then 
62076         if mod(index,4)=0 and index > 1 then 
62078           let speclist$=speclist$&crlf$&"    data "
62080           let helplist$=helplist$&crlf$&"    data "
62082         else if mod(index,2)=0 and index > 1 then 
62084           let speclist$=speclist$&crlf$&"    data "
62086           let helplist$=helplist$&","
62088         else 
62090           let speclist$=speclist$&","
62092           let helplist$=helplist$&","
62094         end if 
62096       end if 
62098     next index
62100 ! 
62102     if udim(mat input_spec$) then 
62104 ! .      ! Read Statements for Input Controls
62106       let fngc("    ! Read Specs for Input Controls")
62108       let fngc("    read mat "&screenname$&"_Input_Spec$")
62110       let fngc("    data "&speclist$)
62112       let fngc
62114       if inputhelp then 
62116         let fngc("    read mat "&screenname$&"_Input_Help$")
62118         let fngc("    data "&helplist$)
62120         let fngc
62122       end if 
62124     end if 
62126 ! 
62128 ! .   ! Build Code to Read Data File
62130     if layoutname$<>"" then 
62132       let fngc("    ! Open Data File")
62134       let fngc("    library """&setting_fileiopath$&""": Fngetfilenumber, Fnopenfile, Fnuniquekey")
62136       let fngc("    let "&layoutname$&"file=Fnopen("""&layoutname$&""",mat "&layoutname$&"$,mat "&layoutname$&",mat Form$)")
62138 ! 
62140       let fngc("    ! Read Data File")
62142       let fngc
62144 ! .      !   ! Read data file
62146 ! .      !   if len(trim$(key$)) then
62148 ! .      !      Read #FarmFile, using form$(FarmFile), key=key$ : mat Farm$ mat Farm
62150 ! .      !      let fnUnpackFarmFile(mat Farm$,mat Farm,mat Detail_Data$)
62152 ! .      !
62154 ! .      !      ! If there's a read event, call it here
62156 ! .      !      let fnReadFarmEvent
62158 ! .      !   else 
62160 ! .      !      ! call init function here
62162 ! .      !      let fnInitFarmEvent
62164 ! .      !   end if
62166     end if 
62168 ! 
62170     let fngc
62172     let fngc("    ! Open Screen")
62174     let fngc("    open #("&screenname$&"_Window:=Fngetfilenumber): """&fnmakewindowspec$(1,1,mat screenio$,mat screenio)&""",display,outin")
62176     let fngc
62178 ! 
62180 ! .   ! Change this code to handle open statements properly
62182 ! 
62184 ! .   ! TODO: Add Code to Open Frames
62186 ! .   !   ! Open any frames here
62188 ! .   !   open #(FrameWindows(1):=fnGetFileNumber) : "row=20,col=20,rows=3,cols=10",display,outin
62190 ! .   !   open #(FrameWindows(2):=fnGetFileNumber) : "row=5,col=60,rows=3,cols=10",display,outin
62192 ! .   !
62194 ! .   !
62196 ! .   !   ! x (Frame windows come from two places -
62198 ! .   !   ! x  1 - Multi-Line Textboxes, Buttons and Captions
62200 ! .   !   ! x  2 - Frame Controls
62202 ! .   !
62204 ! .   !   ! x Open any child screens here by compiling the code for the entire child
62206 ! .   !   ! x  screen following this whole process into a function
62208 ! .   !   ! x  and calling that function from here.
62210 ! .   !   ! x  worry about this later.
62212 ! 
62214 ! .   !
62216 ! .   !   execute "config force visibility on"
62218 ! .   !
62220 ! 
62222     if udim(mat output_data$) then 
62224       if outputhelp then 
62226         let fngc("    print #"&screenname$&"_Window, fields mat "&screenname$&"_Output_Spec$, help mat "&screenname$&"_Output_Help$ : mat "&screenname$&"_Output_Data$")
62228       else 
62230         let fngc("    print #"&screenname$&"_Window, fields mat "&screenname$&"_Output_Spec$ : mat "&screenname$&"_Output_Data$")
62232       end if 
62234     end if 
62236 ! 
62238     for index=1 to udim(mat displaylistview$)
62240       if len(trim$(displaylistview$(index))) then 
62242         let fngc(displaylistview$(index))
62244       end if 
62246     next index
62248 ! 
62250 ! .   !   populate listview here too
62252     let fngc("    ! Main input loop here")
62254     let fngc("    do")
62256     if udim(mat input_data$) then 
62258       if inputhelp then 
62260         let fngc("       rinput #"&screenname$&"_Window, fields mat "&screenname$&"_Input_Spec$, help mat "&screenname$&"_Input_Help$ : mat "&screenname$&"_Input_Data$")
62262       else 
62264         let fngc("       rinput #"&screenname$&"_Window, fields mat "&screenname$&"_Input_Spec$ : mat "&screenname$&"_Input_Data$")
62266       end if 
62268     else 
62270 ! 
62272     end if 
62274     let fngc("       let Function=fkey")
62276 ! 
62278 !      let fnGC("       let fnDetail_Process(Function)")
62280 ! 
62282     let fngc("    loop Until ExitMode")
62284 ! 
62286 ! .   !
62288 ! .   !   execute "config forcevisibility off"
62290 ! .   !
62292 ! .   !   ! Close Files Here
62294 ! .   !   let fnCloseFile(FarmFile)
62296 ! .   !
62298 ! .   !   ! Close all frames
62300 ! .   !   for Index=1 to udim(mat FrameWindows)
62302 ! .   !      close #FrameWindows(Index):
62304 ! .   !   next Index
62306 ! .   !
62308 ! .   !   for Index=1 to udim(mat ScreenWindows)
62310 ! .   !      close #ScreenWindows:
62312 ! .   !   next Index
62314 ! .   !   close #Detail_Window:
62316 ! .   !
62318     let fngc(" fnend")
62320     let fngc
62322 ! 
62324     if layoutname$<>"" then 
62326       let fngc(" !")
62328       let fngc(" ! #Auto"&"number# 99000,10")
62330       let fngc(" OPEN: ! ***** Function To Call Library Openfile And Proc Subs")
62332       let fngc('    def Fnopen(Filename$*255, Mat F$, Mat F, Mat Form$; Inputonly, Keynum, Dont_Sort_Subs, Path$*255, Mat Descr$, Mat Field_Widths,Supress_Prompt,Ignore_Errors,___,Index)')
62334       let fngc('       dim _FileIOSubs$(1)*800, _loadedsubs$(1)*32')
62336       let fngc('       let Fnopen=Fnopenfile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$)')
62338       let fngc('       if srch(_loadedsubs$,uprc$(Filename$))<=0 then : mat _loadedsubs$(UDIM(_loadedsubs$)+1) : _loadedsubs$(UDIM(_loadedsubs$))=uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index')
62340       let fngc('    fnend')
62342     end if 
62344 ! 
62346     let fngc(" !")
62348     let fngc(" ! #Auto"&"number# 99500,10")
62350     let fngc(" IGNORE: continue")
62352 ! 
62354     let setenv("clipboard",generatedcode$)
62356   fnend 
62358 ! 
62360   def fngc(;string$*1024,nocrlf)
62362     if nocrlf then 
62364       let generatedcode$=generatedcode$&string$
62366     else 
62368       let generatedcode$=generatedcode$&string$&crlf$
62370     end if 
62372   fnend 
62374 ! 
65000 !  #AutoNumber# 65000,2
65002 !  =================================================
65004 !  ================= Power Search ==================
65006 !  =================================================
65008 ! 
65010 !  #Define# [[SearchResults]] = "mat PSLV_Source$,mat PSLV_Path$,mat PSLV_Text$,mat PSLV_Color$,mat PSLV_RefNumber"
65012 ! 
65014   dim pslv_spec$
65016   dim pslv_cap$(5)*30,pslv_width(5),pslv_spec$(5)
65018   dim ps_capspec$(22)*30, ps_caption$(22)*30, ps_helptext$(22)*255
65020   dim ps_spec$(15)*30, ps_data$(14)*255, ps_data(1), ps_datahelp$(15)*255
65022   dim pslv_source$(1)*30
65024   dim pslv_path$(1)*255
65026   dim pslv_text$(1)*255
65028   dim pslv_color$(1)
65030   dim pslv_refnumber(1)
65032   dim pslv_selected(1)
65034 ! 
65036   dim screenio_ssubs$(1)*255
65038   dim screenio_nsubs$(1)*255
65040 ! 
65042   dim screenfld_ssubs$(1)*255
65044   dim screenfld_nsubs$(1)*255
65046 ! 
65048   def fnsaveinifile(mat ps_data$)
65050     open #(inifile:=fngetfilenumber) : "name="&setting_screenfolder$&"\powersearch.ini, recl="&str$(udim(mat ps_data$)*255)&", replace",internal,output 
65052     write #inifile, using "form "&str$(udim(mat ps_data$))&"*V 255" : mat ps_data$
65054     close #inifile: 
65056   fnend 
65058 ! 
65060 ! .! Define the Search Screen Here
65062   dim ps_initialize
65064 ! 
65066   def fninitializepowersearch(&pslv_spec$,mat ps_spec$,mat ps_capspec$,mat ps_data$,mat ps_datahelp$,mat ps_caption$,mat pslv_cap$,mat pslv_width,mat pslv_spec$;___,dummy$,inifile)
65068     if ~ps_initialize then 
65070 ! .      ! Read ScreenIO Column Numbers
65072       let fnreadsubs("screenio",mat screenio_ssubs$,mat screenio_nsubs$,dummmy$)
65074       let fnreadsubs("screenfld",mat screenfld_ssubs$,mat screenfld_nsubs$,dummy$)
65076 ! 
65078       execute "config userlevel 0"
65080 ! 
65082 ! .      ! Initialize PowerSearch Results Listview
65084       let pslv_spec$="7,3,LIST 20/83"
65086 ! 
65088 ! .      ! Initialize PowerSearch Results Listview Columns
65090       let pslv_cap$(1)="Source"
65092       let pslv_width(1)=9
65094       let pslv_spec$(1)="C 30"
65096 ! 
65098       let pslv_cap$(2)="Path"
65100       let pslv_width(2)=25
65102       let pslv_spec$(2)="C 255"
65104 ! 
65106       let pslv_cap$(3)="Text"
65108       let pslv_width(3)=80
65110       let pslv_spec$(3)="C 255"
65112 ! 
65114       let pslv_cap$(4)="Color"
65116       let pslv_width(4)=0
65118       let pslv_spec$(4)="C 15"
65120 ! 
65122       let pslv_cap$(5)="Ref"
65124       let pslv_width(5)=0
65126       let pslv_spec$(5)="N 20"
65128 ! 
65130       mat pslv_source$(0)
65132       mat pslv_path$(0)
65134       mat pslv_text$(0)
65136       mat pslv_color$(0)
65138       mat pslv_refnumber(0)
65140 ! 
65142       mat pslv_selected(0)
65144 ! 
65146 ! .      ! Initialize PowerSearch Subscripts
65148       let _ps_searchtext=1
65150       let _ps_folder=2
65152       let _ps_includesubs=3
65154       let _ps_mask=4
65156       let _ps_searchallext=5
65158       let _ps_usealtext=6
65160       let _ps_altext=7
65162       let _ps_altextobject=8
65164       let _ps_altextsource=9
65166       let _ps_screen=10
65168       let _ps_layout=11
65170       let _ps_function=12
65172       let _ps_path=13
65174       let _ps_helperlib=14
65176       let _ps_searchresults=15
65178 ! 
65180 ! .      ! Initialize PowerSearch Input Specs
65182       let ps_spec$(_ps_searchtext)="5,24,42/SEARCH 255,/W:W,7,3"
65184       let ps_spec$(_ps_folder)="4,24,15/V 255,/W:W"
65186       let ps_spec$(_ps_includesubs)="4,40,12/CHECK 18"
65188       let ps_spec$(_ps_mask)="4,59,7/V 80,/W:W"
65190 ! 
65192       let ps_spec$(_ps_searchallext)="3,24,12/CHECK 18"
65194       let ps_spec$(_ps_usealtext)="3,40,6/CHECK 25"
65196       let ps_spec$(_ps_altext)="3,47,CU 3,/W:W"
65198       let ps_spec$(_ps_altextobject)="3,51,7/RADIO 25,1"
65200       let ps_spec$(_ps_altextsource)="3,59,7/RADIO 25,1"
65202 ! 
65204       let ps_spec$(_ps_screen)="1,4,13/CHECK 25"
65206       let ps_spec$(_ps_layout)="2,4,13/CHECK 25"
65208       let ps_spec$(_ps_function)="3,4,13/CHECK 25"
65210       let ps_spec$(_ps_path)="4,4,13/CHECK 25"
65212       let ps_spec$(_ps_helperlib)="5,4,13/CHECK 25"
65214 ! 
65216       let ps_spec$(_ps_searchresults)=pslv_spec$&",ROWCNT,SEL"
65218 ! 
65220       let ps_datahelp$(_ps_searchtext)="3;Enter the text to search for.;"
65222       let ps_datahelp$(_ps_folder)="3;Enter an alternate path to search. Use the BR "":"" operator to specify an absolute path. When using an absolute path, we are limited to a Text Only Search. If you want to search inside BR Object Files, you must specify a relative path.;"
65224       let ps_datahelp$(_ps_includesubs)="3;Search in Subdirectories. May take a while. Modify dirlist.sch to filter the search directories.;"
65226       let ps_datahelp$(_ps_mask)="3;This is the mask. Do not enter anything other then letters and the traditional * and ? mask operators. Do not enter a file extension or a path in this field.;"
65228       let ps_datahelp$(_ps_searchallext)="3;Search all other files of all extensions. The program will test each file to if it is an ASCII file, and it will only search them if they are ascii files.;"
65230       let ps_datahelp$(_ps_usealtext)="3;Search inside the specified folder for files of this alternate extension.;"
65232       let ps_datahelp$(_ps_altext)="3;Alternate Extension to Search\nEnter extension only (LIB and not *.LIB);"
65234       let ps_datahelp$(_ps_altextobject)="3;Treat Alternate Extension files as BR Program Files;"
65236       let ps_datahelp$(_ps_altextsource)="3;Treat Alternate Extension files as Text Files;"
65238       let ps_datahelp$(_ps_screen)="3;Search the Screen Files;"
65240       let ps_datahelp$(_ps_layout)="3;Search the File Layouts;"
65242       let ps_datahelp$(_ps_function)="3;Search the Screen Functions;"
65244       let ps_datahelp$(_ps_path)="3;Search the specified folder for .brs, .wbs, .br and .wb files.;"
65246       let ps_datahelp$(_ps_helperlib)="3;Search your compiled Screen Helper Functions. You shouldn't really be searching for these. Search your ScreenIO Screen Functions instead.;"
65248       let ps_datahelp$(_ps_searchresults)="3;;"
65250 ! 
65252 ! .      ! Initialize PowerSearch Input Data
65254       if exists(""&setting_screenfolder$&"\powersearch.ini") then 
65256         open #(inifile:=fngetfilenumber) : "name="&setting_screenfolder$&"\powersearch.ini",internal,input,sequential 
65258         read #inifile, using "form "&str$(udim(mat ps_data$))&"*V 255" : mat ps_data$ error ignore
65260         if file(inifile)<>0 then 
65262           close #inifile: 
65264           goto USEDEFAULTS
65266         end if 
65268         close #inifile: 
65270       else 
65272 USEDEFAULTS: ! use default settings, read failed.
65274         let ps_data$(_ps_folder)="."
65276         let ps_data$(_ps_includesubs)="^Incl Subfolders"
65278         let ps_data$(_ps_mask)="*"
65280         let ps_data$(_ps_searchallext)="Search All Exts"
65282         let ps_data$(_ps_screen)="^Screen Files"
65284         let ps_data$(_ps_layout)="^File Layouts"
65286         let ps_data$(_ps_function)="^Screen Functions"
65288         let ps_data$(_ps_path)="^Folder"
65290         let ps_data$(_ps_helperlib)="Compiled Screens"
65292         let ps_data$(_ps_usealtext)="Alt Ext: "
65294         let ps_data$(_ps_altext)="LIB"
65296         let ps_data$(_ps_altextobject)="^BR File"
65298         let ps_data$(_ps_altextsource)="Text File"
65300       end if 
65302 ! 
65304 ! .      ! Initialize PowerSearch Buttons and Caption Specs
65306       let ps_capspec$(1)="1,30,CC 26" ! Power Search
65308       let ps_capspec$(2)="5,17,6/CR 8" ! Search:
65310       let ps_capspec$(3)="4,17,6/CR 8" ! Folder:
65312       let ps_capspec$(4)="5,67,CC 9,/W:W,B35" ! Search
65314       let ps_capspec$(5)="5,77,9/CC 9,/W:W,B34" ! Clear All
65316       let ps_capspec$(6)="28,3,10/CC 17,/W:W,B36" ! Launch Item
65318       let ps_capspec$(7)="28,14,17/CC 26,/W:W,B37" ! Affected Screen List
65320 ! 
65322       let ps_capspec$(8)="28,68,8/CC 11,/W:W,B38" ! Export List
65324       let ps_capspec$(9)="28,77,CC 8,/W:W,B39" ! Print List
65326 ! 
65328       let ps_capspec$(10)="28,37,8/CC 14,/W:W,B40" ! Load Results
65330       let ps_capspec$(11)="28,46,8/CC 12,/W:W,B41" ! Save Results
65332       let ps_capspec$(12)="28,55,8/CC 11,/W:W,B42" ! Save As...
65334 ! 
65336       let ps_capspec$(13)="3,69,CC 1,/#000000:#00FF00,B43" ! Mark Green
65338       let ps_capspec$(14)="3,71,CC 1,/#000000:#FFFF00,B51" ! Mark Yellow
65340       let ps_capspec$(15)="3,73,CC 1,/#000000:#FFFFFF,B45" ! Mark White
65342 ! 
65344       let ps_capspec$(16)="3,79,CC 1,/#FFFFFF:#008000,B46" ! Clear Green
65346       let ps_capspec$(17)="3,81,CC 1,/#FFFFFF:#808000,B47" ! Clear Yellow
65348       let ps_capspec$(18)="3,83,CC 1,/#000000:#FFFFFF,B48" ! Clear White
65350 ! 
65352       let ps_capspec$(19)="4,77,9/CC 7,/W:W,B49" ! Clear Item
65354       let ps_capspec$(20)="4,67,9/CC 11,/W:W,B50" ! Sort Colors
65356 ! 
65358       let ps_capspec$(21)="4,54,4/CR 6" ! Mask:
65360       let ps_capspec$(22)="2,69,14/CC 15,/W:W,B52" ! Save Ini
65362 ! 
65364 ! .      ! Initialize PowerSearch Buttons and Captions
65366       let ps_caption$(1)="Power Search"
65368       let ps_caption$(2)="Search: "
65370       let ps_caption$(3)="Folder: "
65372       let ps_caption$(4)="Search"
65374       let ps_caption$(5)="Clear All"
65376       let ps_caption$(6)="Launch"
65378       let ps_caption$(7)="Affected Screens"
65380       let ps_caption$(8)="Export"
65382       let ps_caption$(9)="Print"
65384       let ps_caption$(10)="Load.."
65386       let ps_caption$(11)="Save"
65388       let ps_caption$(12)="Save As.."
65390       let ps_caption$(13)=" " ! Mark Green
65392       let ps_caption$(14)=" " ! Mark Yellow
65394       let ps_caption$(15)=" " ! Mark White
65396       let ps_caption$(16)="x" ! Clear Green
65398       let ps_caption$(17)="x" ! Clear Yellow
65400       let ps_caption$(18)="x" ! Clear White
65402       let ps_caption$(19)="Clear"
65404       let ps_caption$(20)="Sort Colors"
65406       let ps_caption$(21)="Mask: "
65408       let ps_caption$(22)="Set Defaults"
65410 ! 
65412       mat ps_helptext$(1:3)=("3;;")
65414       let ps_helptext$(4)="3;Search. Search without clearing to add to the previous results.;"
65416       let ps_helptext$(5)="3;Clear the list to preform a new search.;"
65418       let ps_helptext$(6)="3;Launches the selected items. If the item is a file, launch it in MyEdit. If its a screen, Load it in the background.;"
65420       let ps_helptext$(7)="3;Opens a window showing all the screens that use the currently selected items.;"
65422       let ps_helptext$(8)="3;Exports the Search Results to a Text File.;"
65424       let ps_helptext$(9)="3;Prints the Search Results to the Printer.;"
65426       let ps_helptext$(10)="3;Loads a previously saved Search Results file so you can work with it.;"
65428       let ps_helptext$(11)="3;Saves the Results to an internal file so you can work with them later.;"
65430       let ps_helptext$(12)="3;Saves the Results to an internal file so you can work with them later.;"
65432       let ps_helptext$(13)="3;Mark these lines Green.;"
65434       let ps_helptext$(14)="3;Mark these lines Yellow.;"
65436       let ps_helptext$(15)="3;Mark these lines White.;"
65438       let ps_helptext$(16)="3;Clear all Green Items.;"
65440       let ps_helptext$(17)="3;Clear all Yellow Items.;"
65442       let ps_helptext$(18)="3;Clear all White Items.;"
65444       let ps_helptext$(19)="3;Clear Selected Items.;"
65446       let ps_helptext$(20)="3;Sort the list based on color.;"
65448       let ps_helptext$(21)="3;;"
65450       let ps_helptext$(22)="3;Save the Search Settings to be used as defaults later.;"
65452 ! 
65454       let ps_initialize=1
65456     end if 
65458   fnend 
65460 ! 
65462   def fnpowersearch(;___,function,powersearch,rowcount)
65464 ! 
65466 ! .   ! Open the child window
65468     open #(powersearch:=fngetfilenumber): "srow=4,scol=10,rows=29,cols=87,Border=s", display, outin 
65470 ! 
65472     let fninitializepowersearch(pslv_spec$,mat ps_spec$,mat ps_capspec$,mat ps_data$,mat ps_datahelp$,mat ps_caption$,mat pslv_cap$,mat pslv_width,mat pslv_spec$)
65474 ! 
65476     print #powersearch, fields mat ps_capspec$, help mat ps_helptext$ : mat ps_caption$
65478     print #powersearch, fields pslv_spec$&",HEADERS" : (mat pslv_cap$, mat pslv_width, mat pslv_spec$)
65480 ! 
65482 ! .   ! Predraw listview if any data belongs in it
65484     let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65486     do 
65488       print #powersearch, fields mat ps_spec$(1:udim(mat ps_spec$)-1) : mat ps_data$
65490       if udim(mat pslv_source$) then 
65492         input #powersearch, fields mat ps_spec$, help mat ps_datahelp$ : mat ps_data$,rowcount
65494         mat pslv_selected(rowcount)
65496         input #powersearch, fields pslv_spec$&",ROWSUB,SEL,NOWAIT" : mat pslv_selected
65498       else 
65500         input #powersearch, fields mat ps_spec$(1:udim(mat ps_spec$)-1), help mat ps_datahelp$ : mat ps_data$
65502       end if 
65504 ! 
65506       let function=fkey
65508 ! 
65510       if function = 34 then ! #Select# Function #Case# 34
65512 ! .         ! Clear List
65514         let fnclearresults(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65516         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65518 ! 
65520       else if function = 35 then ! #Case# 35
65522 ! .         ! Search
65524         let fnsearch(mat ps_data$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65526         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65528 ! 
65530       else if function = 0 then ! #Case# 0
65532         if udim(mat pslv_source$)=0 then 
65534           let fnsearch(mat ps_data$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65536           let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65538         end if 
65540 ! 
65542       else if function = 36 then ! #Case# 36
65544 ! .         ! Jump To Item
65546         let fnjumpto(mat pslv_selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65548 ! 
65550       else if function = 37 then ! #Case# 37
65552 ! .         ! Affected Screen List
65554         let fnaffectedscreenlist(mat pslv_selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65556 ! 
65558       else if function = 38 then ! #Case# 38
65560 ! .         ! Export
65562         let fnprintlisttofile(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65564       else if function = 39 then ! #Case# 39
65566 ! .         ! Print
65568         let fnprintlisttoprinter(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65570 ! 
65572       else if function = 40 then ! #Case# 40
65574 ! .         ! Load
65576         let fnloadresultsfile(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65578         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65580       else if function = 41 then ! #Case# 41
65582 ! .         ! Save
65584         let fnsaveresultsfile(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65586       else if function = 42 then ! #Case# 42
65588 ! .         ! Save As..
65590         let fnsaveresultsfileas(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65592 ! 
65594       else if function = 43 then ! #Case# 43
65596 ! .         ! Mark Green
65598         let fnmarkselection("#00FF00",mat pslv_selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65600         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,1)
65602       else if function = 51 then ! #Case# 51
65604 ! .         ! Mark Yellow
65606         let fnmarkselection("#FFFF00",mat pslv_selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65608         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,1)
65610       else if function = 45 then ! #Case# 45
65612 ! .         ! Mark White
65614         let fnmarkselection("#FFFFFF",mat pslv_selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65616         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,1)
65618       else if function = 46 then ! #Case# 46
65620 ! .         ! Clear Green
65622         let fnclearselection("#00FF00",mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65624         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65626       else if function = 47 then ! #Case# 47
65628 ! .         ! Clear Yellow
65630         let fnclearselection("#FFFF00",mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65632         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65634       else if function = 48 then ! #Case# 48
65636 ! .         ! Clear White
65638         let fnclearselection("#FFFFFF",mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65640         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65642       else if function = 49 then ! #Case# 49
65644 ! .         ! Clear Item
65646         let fnclearitem(mat pslv_selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65648         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65650       else if function = 50 then ! #Case# 50
65652 ! .         ! Sort Colors
65654         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65656         print #powersearch, fields pslv_spec$&",SORT" : 4
65658 ! 
65660       else if function = 52 then ! #Case# 52
65662         let fnsaveinifile(mat ps_data$)
65664 ! 
65666       end if  ! #End Select#
65668 ! 
65670     loop until function=99 or function=93 or fkey=93
65672 ! 
65674     close #powersearch: 
65676   fnend 
65678 ! 
65680   dim pslv_start(1)
65682   dim pslv_end(1)
65684   dim pslv_attr$(1)
65686   dim pslv_outcolor$(1)
65688 ! 
65690   def fnupdateresults(listspec$*30,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;coloronly,___,index,bold,lastsource$*30,lastpath$*255)
65692     if ~coloronly then 
65694       print #powersearch, fields listspec$&",=" : (mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65696     end if 
65698 ! 
65700     mat pslv_start(0) : mat pslv_end(0) : mat pslv_attr$(0) : let lastindex=0
65702     mat pslv_outcolor$(udim(mat pslv_color$))=pslv_color$
65704 ! 
65706     for index=1 to udim(mat pslv_color$)
65708 ! .      ! Alternate bold and not bold every time source changes.
65710       if lastsource$<>pslv_source$(index) or lastpath$<>fncomparepath$(pslv_source$(index),pslv_path$(index)) then 
65712         let lastsource$=pslv_source$(index)
65714         let lastpath$=fncomparepath$(pslv_source$(index),pslv_path$(index))
65716         let bold=~bold
65718       end if 
65720 ! 
65722 ! .      ! Specify the color from color$
65724       if bold then 
65726         let pslv_outcolor$(index)="/#000000:"&pslv_color$(index)
65728       else 
65730         let pslv_outcolor$(index)="/#000000:"&srep$(pslv_color$(index),"FF","E0")
65732       end if 
65734     next index
65736 ! 
65738 ! .   ! Borrow the SetColors function from ScreenIO
65740     let fnsetcolors(mat pslv_outcolor$,mat pslv_start,mat pslv_end,mat pslv_attr$)
65742 ! 
65744     print #powersearch, fields listspec$&",attr" : (mat pslv_start,mat pslv_end,mat pslv_attr$)
65746 ! 
65748     print #powersearch, fields "27,3,C 30" : str$(udim(mat pslv_source$))&" results found..."
65750   fnend 
65752 ! 
65754   def fncomparepath$*255(source$*30,path$*255)
65756     if lwrc$(source$)="screen" then 
65758       let fncomparepath$=path$(1:pos(path$,"\"))
65760     else 
65762       let fncomparepath$=path$
65764     end if 
65766   fnend 
65768 ! 
65770   def fnmarkselection(color$,mat selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index)
65772     for index=1 to udim(mat selected)
65774       let pslv_color$(selected(index))=color$
65776     next index
65778   fnend 
65780   def fnclearselection(color$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index)
65782     if (2==msgbox("This will remove any "&fncolorname$(color$)&" items from the list. Are you sure?","Delete All"&fncolorname$(color$),"yN","QST")) then 
65784       let index=0
65786       do while index<udim(mat pslv_color$)
65788         let index+=1
65790         if pslv_color$(index)=color$ then 
65792 ! .            ! Remove Entry from List
65794           let fnremoveitem(index,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65796           let index-=1
65798         end if 
65800       loop 
65802     end if 
65804     mat pslv_selected(0)
65806   fnend 
65808   def fnclearitem(mat selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index,jndex)
65810     if (2==msgbox("This will remove all the currently selected items from the list. Are you sure?","Delete Current Selection","yN","QST")) then 
65812       for index=1 to udim(mat selected)
65814 ! .         ! Remove Selected(Index) from list
65816         let fnremoveitem(selected(index),mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
65818         for jndex=index+1 to udim(mat selected)
65820           let selected(jndex)-=1
65822         next jndex
65824       next index
65826     end if 
65828     mat selected(0)
65830   fnend 
65832 ! 
65834   dim alreadydone$(1)*255
65836 ! .! Jump To Item
65838   def fnjumpto(mat selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index,doneascreen,doneahelperlib,screen$,firstslashpos)
65840     mat alreadydone$(0)
65842     for index=1 to udim(mat selected)
65844       if lwrc$(trim$(pslv_source$(selected(index))))="screen" then 
65846         let screen$=pslv_path$(selected(index))(1:(firstslashpos:=pos(pslv_path$(selected(index)),"\"))-1)
65848         let fnlaunchscreen(trim$(screen$))
65850 ! 
65852 ! .         !if ~DoneAScreen then
65854 ! .         !   ! if this item is a screen, load the screen, redraw it, then come back here.
65856 ! .         !   if Fnoktoproceed(Mat Screenio$,Mat Screenio,mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$, mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat Multiselect, mat UserData$, mat GridLines) then
65858 ! .         !      let Screen$=PSLV_Path$(Selected(Index))(1:(FirstSlashPos:=pos(PSLV_Path$(Selected(Index)),"\"))-1)
65860 ! .         !      if Trim$(Screen$)<>"" then
65862 ! .         !         if Fnreadscreen(Screen$,Mat Screenio$,Mat Screenio,mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$, mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat Multiselect, mat UserData$, mat GridLines) then
65864 ! .         !            let fnRedrawScreen(mat ScreenIO$,mat ScreenIO,mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$, mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat Multiselect, mat UserData$, mat GridLines)
65866 ! .         !
65868 ! .         !            if pos(PSLV_Path$(Selected(Index)),"\",FirstSlashPos+1) then ! Its from screenflds
65870 ! .         !               let Mode=Inputeditormode
65872 ! .         !               let Control=PSLV_RefNumber(Selected(Index))
65874 ! .         !            else ! its from ScreenIO
65876 ! .         !               if pos(lwrc$(PSLV_Path$(Selected(Index))),"fn$") then
65878 ! .         !                  let Mode=SelectEventsMode
65880 ! .         !               else 
65882 ! .         !                  let Mode=InputAttributesMode
65884 ! .         !                  let Control=PSLV_RefNumber(Selected(Index))
65886 ! .         !               end if
65888 ! .         !            end if
65890 ! .         !            let DoneAScreen=1
65892 ! .         !         else 
65894 ! .         !            let Msgbox("Error Reading Screen - Screen not  found","Error","OK","ERR")
65896 ! .         !         end if
65898 ! .         !      end if
65900 ! .         !   end if
65902 ! .         !end if
65904       else if lwrc$(trim$(pslv_source$(selected(index))))="helper library" then 
65906         if ~doneahelperlib then 
65908           let msgbox("Don't Edit your Compiled Helper Libraries. Instead, edit the functions they're built from.","Warning","Ok","EXCL")
65910           let doneahelperlib=1
65912         end if 
65914       else if lwrc$(trim$(pslv_source$(selected(index))))="layout" and ~pos(pslv_path$(selected(index)),".") then 
65916 ! .         ! if this item is a function, launch it in editor
65918         if srch(mat alreadydone$,lwrc$(trim$(pslv_path$(selected(index)))))<1 then 
65920 ! .         ! Launch it in notepad
65922           let sleep(.1)
65924           execute "system -C -M start notepad "&os_filename$(trim$(pslv_path$(selected(index)))) error ignore
65926 ! 
65928           mat alreadydone$(udim(mat alreadydone$)+1)
65930           let alreadydone$(udim(mat alreadydone$))=lwrc$(trim$(pslv_path$(selected(index))))
65932         end if 
65934       else 
65936 ! .         ! if this item is a function, launch it in editor
65938         if srch(mat alreadydone$,lwrc$(trim$(pslv_path$(selected(index)))))<1 then 
65940           let sleep(.1)
65942 ! .            ! if its a .BR file then
65944           if ".br"=trim$(lwrc$(pslv_path$(selected(index)))(pos(pslv_path$(selected(index)),".",-1):len(pslv_path$(selected(index))))) or ".wb"=trim$(lwrc$(pslv_path$(selected(index)))(pos(pslv_path$(selected(index)),".",-1):len(pslv_path$(selected(index))))) then 
65946 ! .               ! Launch it in BR
65948             execute "system -C -M "&fngetbrexe$&" -"&fngetwbcfg$&" load "&trim$(pslv_path$(selected(index)))
65950           else if lwrc$("."&ps_data$(_ps_altext))=trim$(lwrc$(pslv_path$(selected(index)))(pos(pslv_path$(selected(index)),".",-1):len(pslv_path$(selected(index))))) and ps_data$(_ps_altextobject)(1:1)="^" then 
65952 ! .               ! Its a BR file with the alt ext
65954             execute "system -C -M "&fngetbrexe$&" -"&fngetwbcfg$&" load "&trim$(pslv_path$(selected(index)))
65956           else 
65958 ! .               ! Launch it in default editor
65960             execute "system -C -M start "&os_filename$(trim$(pslv_path$(selected(index)))) error ignore
65962           end if 
65964 ! 
65966           mat alreadydone$(udim(mat alreadydone$)+1)
65968           let alreadydone$(udim(mat alreadydone$))=lwrc$(trim$(pslv_path$(selected(index))))
65970         end if 
65972       end if 
65974     next index
65976   fnend 
65978 ! 
65980   def fnredrawscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;notpowersearch)
65982     if ~(fnsameas(mat displayref_screenio$,mat screenio$) and fnsamea(mat displayref_screenio,mat screenio)) then 
65984 ! 
65986       close #powersearch: 
65988       let fnredrawentirescreen(wtoolbar,wdebug,weditor,mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
65990 ! 
65992       mat displayref_screenio$=screenio$
65994       mat displayref_screenio=screenio
65996 ! 
65998       let fnrepopulatefieldslistview(screenio$(si_filelay))
66000       let displayref_screenio$(si_filelay)=screenio$(si_filelay)
66002 ! 
66004       if ~notpowersearch then 
66006 ! .         ! Open the child window
66008         open #(powersearch:=fngetfilenumber): "srow=4,scol=10,rows=29,cols=87,Border=s", display, outin 
66010 ! 
66012         let fninitializepowersearch(pslv_spec$,mat ps_spec$,mat ps_capspec$,mat ps_data$,mat ps_datahelp$,mat ps_caption$,mat pslv_cap$,mat pslv_width,mat pslv_spec$)
66014 ! 
66016         print #powersearch, fields mat ps_capspec$, help mat ps_helptext$ : mat ps_caption$
66018         print #powersearch, fields pslv_spec$&",HEADERS" : (mat pslv_cap$, mat pslv_width, mat pslv_spec$)
66020 ! 
66022 ! .         ! Predraw listview if any data belongs in it
66024         let fnupdateresults(pslv_spec$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66026       end if 
66028     end if 
66030     let fnrenderscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
66032   fnend 
66034 ! 
66036   dim affectedscreens$(1)
66038   dim as_caption$(1)
66040   dim as_width(1)
66042   dim as_spec$(1)
66044 ! 
66046   dim af_selection(1)
66048 ! 
66050   def fnaffectedscreenlist(mat selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index,window,afselcount,function,savescreencode$,filename$*255)
66052 ! 
66054 ! .   ! Build list of affected screens
66056     mat affectedscreens$(0)
66058     let fnfindaffectedscreens(mat affectedscreens$,mat selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66060 ! 
66062     if udim(mat affectedscreens$)=0 then 
66064       let msgbox("No screens are affected.","Nothing Found")
66066     else 
66068 ! .      ! Define Listview
66070       let as_caption$(1)="Affected Screens"
66072       let as_width(1)=30
66074       let as_spec$(1)="CC 18"
66076 ! 
66078 ! .      ! Open Window
66080       open #(window:=fngetfilenumber) : "srow=5,scol=30,rows=15,cols=30,border=s",display,outin 
66082 ! 
66084 ! .      ! Paint Window
66086       print #window, fields "1,1,LIST 14/30,HEADERS" : (mat as_caption$,mat as_width,mat as_spec$)
66088       print #window, fields "1,1,LIST 14/30,=" : (mat affectedscreens$)
66090       print #window, fields "15,1,14/CC 16,/W:W,B30;15,16,CC 14,/W:W,B31" : "Recompile Screen","Load Screen"
66092 ! 
66094       do 
66096         input #window, fields "1,1,LIST 14/30,ROWCNT,SEL" : afselcount
66098         let function=fkey
66100         mat af_selection(afselcount)
66102         input #window, fields "1,1,LIST 14/30,ROWSUB,SEL,NOWAIT" : mat af_selection
66104 ! 
66106         if function=30 then ! Recompile Selected Screens
66108           let savescreencode$=static_screenname$
66110 ! 
66112           if exists("compile[SESSION].$$$") then 
66114             execute "free compile[SESSION].$$$"
66116           end if 
66118 ! 
66120           let failedlist$=""
66122           for index=1 to udim(mat af_selection)
66124             if fnreadscreen(affectedscreens$(af_selection(index)),mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
66126               let fncompilehelperlibrary(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,filename$)
66128               let fnwaitforfile(failedlist$,affectedscreens$(af_selection(index)),filename$)
66130             end if 
66132           next index
66134 ! 
66136           if len(failedlist$) then 
66138             let msgbox("The following screens took a long time and may have had errors:"&chr$(13)&chr$(13)&failedlist$(3:9999))
66140           end if 
66142 ! 
66144           let fnreadscreen(savescreencode$,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
66146         end if 
66148 ! 
66150         if function=31 then ! Load Screen
66152           let fnlaunchscreen(trim$(affectedscreens$(af_selection(1))))
66154 ! 
66156 ! .         !   if Fnoktoproceed(Mat Screenio$,Mat Screenio,mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$, mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat Multiselect, mat UserData$, mat GridLines) then
66158 ! .         !      if Fnreadscreen(AffectedScreens$(AF_Selection(1)),Mat Screenio$,Mat Screenio,mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$, mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat Multiselect, mat UserData$, mat GridLines) then
66160 ! .         !         let fnRedrawScreen(mat ScreenIO$,mat ScreenIO,mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$, mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat Multiselect, mat UserData$, mat GridLines)
66162 ! 
66164 ! .         !         ! Repaint Window
66166 ! .         !         open #(Window:=fnGetFileNumber) : "srow=5,scol=30,rows=15,cols=30,border=s",display,outin
66168 ! .         !         print #Window, fields "1,1,LIST 14/30,HEADERS" : (mat AS_Caption$,mat AS_Width,mat AS_Spec$)
66170 ! .         !         print #Window, fields "1,1,LIST 14/30,=" : (mat AffectedScreens$)
66172 ! .         !         print #Window, fields "15,1,14/CC 16,/W:W,B30;15,16,CC 14,/W:W,B31" : "Recompile Screen","Load Screen"
66174 ! 
66176 ! .         !      else 
66178 ! .         !         let Msgbox("Error Reading Screen - Screen not  found","Error","OK","ERR")
66180 ! .         !      end if
66182 ! .         !   end if
66184         end if 
66186 ! 
66188       loop until function=99 or function=93 or fkey=93
66190       close #window: 
66192     end if 
66194   fnend 
66196 ! 
66198   def fnfindaffectedscreens(mat affectedscreens$,mat selected,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,function$*255,index,jndex)
66200     for index=1 to udim(mat selected)
66202       if lwrc$(trim$(pslv_source$(selected(index)))) = "screen" then ! #Select# lwrc$(trim$(PSLV_Source$(Selected(Index)))) #Case# "screen"
66204 ! .         ! if its a screen, throw it on the list
66206         let fnaddafscreen(pslv_path$(selected(index))(1:pos(pslv_path$(selected(index)),"\")-1),mat affectedscreens$)
66208 ! 
66210       else if lwrc$(trim$(pslv_source$(selected(index)))) = "function" then ! #Case# "function"
66212 ! .         ! If its a function, search for it in all the screens
66214         let function$=fnfunctionstring$(pslv_path$(selected(index)))
66216 ! 
66218         mat searchscreenio$(udim(mat screenio$))
66220         mat searchscreenio(udim(mat screenio))
66222         mat searchscreenfld$(udim(mat screenfld$))
66224         mat searchscreenfld(udim(mat screenfld))
66226 ! 
66228         restore #fscreenio: 
66230         restore #fscreenfld: 
66232 ! 
66234         do until file(fscreenio)
66236           read #fscreenio, using form$(fscreenio), release: mat searchscreenio$, mat searchscreenio eof ignore
66238           if file(fscreenio)=0 then 
66240             for jndex=si_enterfn to si_exitfn
66242               if lwrc$(trim$(searchscreenio$(jndex)))=lwrc$(trim$(function$)) then 
66244                 let fnaddafscreen(searchscreenio$(si_screencode),mat affectedscreens$)
66246               end if 
66248             next jndex
66250           end if 
66252         loop 
66254         do until file(fscreenfld)
66256           read #fscreenfld, using form$(fscreenfld), release: mat searchscreenfld$, mat searchscreenfld eof ignore
66258           if file(fscreenfld)=0 then 
66260             if lwrc$(trim$(searchscreenfld$(sf_function)))=lwrc$(trim$(function$)) then 
66262               let fnaddafscreen(searchscreenfld$(sf_screencode),mat affectedscreens$)
66264             end if 
66266             if lwrc$(trim$(searchscreenfld$(sf_cnvrtin)))=lwrc$(trim$(function$)) then 
66268               let fnaddafscreen(searchscreenfld$(sf_screencode),mat affectedscreens$)
66270             end if 
66272             if lwrc$(trim$(searchscreenfld$(sf_cnvrtout)))=lwrc$(trim$(function$)) then 
66274               let fnaddafscreen(searchscreenfld$(sf_screencode),mat affectedscreens$)
66276             end if 
66278           end if 
66280         loop 
66282 ! 
66284       else if lwrc$(trim$(pslv_source$(selected(index)))) = "helper library" then ! #Case# "helper library"
66286 ! .         ! if its a helper lib throw the screen on the list
66288         let fnaddafscreen(pslv_path$(selected(index))(pos(pslv_path$(selected(index)),"\",-1)+1:pos(pslv_path$(selected(index)),".",-1)-1),mat affectedscreens$)
66290 ! 
66292       else if lwrc$(trim$(pslv_source$(selected(index)))) = "layout" then ! #Case# "layout"
66294 ! .         ! If its a file layout Search ScreenIO, add all screens that have a match in ScreenIO(si_filelay)
66296         let function$=pslv_path$(selected(index))(pos(pslv_path$(selected(index)),"\",-1)+1:len(pslv_path$(selected(index))))
66298 ! 
66300         mat searchscreenio$(udim(mat screenio$))
66302         mat searchscreenio(udim(mat screenio))
66304 ! 
66306         restore #fscreenio: 
66308         restore #fscreenfld: 
66310 ! 
66312         do until file(fscreenio)
66314           read #fscreenio, using form$(fscreenio), release: mat searchscreenio$, mat searchscreenio eof ignore
66316           if file(fscreenio)=0 then 
66318             if lwrc$(trim$(searchscreenio$(si_filelay)))=lwrc$(trim$(function$)) then 
66320               let fnaddafscreen(searchscreenio$(si_screencode),mat affectedscreens$)
66322             end if 
66324           end if 
66326         loop 
66328 ! 
66330       end if  ! #End Select#
66332     next index
66334   fnend 
66336 ! 
66338   def fnaddafscreen(screen$,mat screens$)
66340     if srch(mat screens$,lwrc$(trim$(screen$)))<1 then 
66342       mat screens$(udim(mat screens$)+1)
66344       let screens$(udim(mat screens$))=lwrc$(trim$(screen$))
66346     end if 
66348   fnend 
66350 ! 
66352   def fnloadresultsfile(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66354 ! .   ! Browse for a file
66356     open #(filenumber:=fngetfilenumber): "name=open:.\*.res", internal, input, sequential error ignore
66358     if filenumber and file(filenumber)>-1 then 
66360       let fnloadresults(filenumber,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66362       let resultsfilename$=file$(filenumber)
66364       close #filenumber: 
66366     end if 
66368   fnend 
66370   def fnsaveresultsfile(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,filenumber)
66372     if trim$(resultsfilename$)="" then 
66374       let fnsaveresultsfileas(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66376     else 
66378       open #(filenumber:=fngetfilenumber): "name="&resultsfilename$&", recl=1024, replace", internal, output, sequential 
66380       if filenumber and file(filenumber)>-1 then 
66382         let fnsaveresults(filenumber,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66384         close #filenumber: 
66386       end if 
66388     end if 
66390   fnend 
66392   dim resultsfilename$*255
66394   def fnsaveresultsfileas(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,filenumber)
66396 ! .   ! Browse for a file
66398     open #(filenumber:=fngetfilenumber): "name=save:.\*.res, recl=1024,replace", internal, output, sequential error ignore
66400     if filenumber and file(filenumber)>-1 then 
66402       let fnsaveresults(filenumber,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66404       let resultsfilename$=file$(filenumber)
66406       close #filenumber: 
66408     end if 
66410   fnend 
66412   def fnsaveresults(filenumber,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index)
66414     write #filenumber, using "form BH 3" : udim(mat pslv_source$)
66416     for index=1 to udim(mat pslv_source$)
66418       write #filenumber, using "form V 30,V 255,V 255,V 15,N 20" : pslv_source$(index),pslv_path$(index),pslv_text$(index),pslv_color$(index),pslv_refnumber(index)
66420     next index
66422   fnend 
66424   def fnloadresults(filenumber,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,count,index)
66426     read #filenumber, using "form BH 3" : count
66428     mat pslv_source$(count)
66430     mat pslv_path$(count)
66432     mat pslv_text$(count)
66434     mat pslv_color$(count)
66436     mat pslv_refnumber(count)
66438     for index=1 to count
66440       read #filenumber, using "form V 30,V 255,V 255,V 15,N 20" : pslv_source$(index),pslv_path$(index),pslv_text$(index),pslv_color$(index),pslv_refnumber(index)
66442     next index
66444   fnend 
66446 ! 
66448   def fnclearresults(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66450     if (2==msgbox("This will remove all results from the list. Are you sure?","Clear all Results","yN","QST")) then 
66452       mat pslv_source$(0)
66454       mat pslv_path$(0)
66456       mat pslv_text$(0)
66458       mat pslv_color$(0)
66460       mat pslv_refnumber(0)
66462       mat pslv_selected(0)
66464     end if 
66466   fnend 
66468 ! 
66470   def fnremoveitem(item,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index)
66472     for index=item to udim(mat pslv_source$)-1
66474       let pslv_source$(index)=pslv_source$(index+1)
66476       let pslv_path$(index)=pslv_path$(index+1)
66478       let pslv_text$(index)=pslv_text$(index+1)
66480       let pslv_color$(index)=pslv_color$(index+1)
66482       let pslv_refnumber(index)=pslv_refnumber(index+1)
66484     next index
66486     mat pslv_source$(udim(mat pslv_source$)-1)
66488     mat pslv_path$(udim(mat pslv_source$))
66490     mat pslv_text$(udim(mat pslv_source$))
66492     mat pslv_color$(udim(mat pslv_source$))
66494     mat pslv_refnumber(udim(mat pslv_source$))
66496   fnend 
66498 ! 
66500   dim warned
66502 ! 
66504   def fnaddresult(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,source$*30,path$*255,text$*255;refnumber,color$,___,index)
66506     let index=udim(mat pslv_source$)+1
66508     if fn42 or index<4100 then 
66510       mat pslv_source$(index)
66512       mat pslv_path$(index)
66514       mat pslv_text$(index)
66516       mat pslv_color$(index)
66518       mat pslv_refnumber(index)
66520 ! 
66522       if color$="" then let color$="#FFFFFF"
66524 ! 
66526       let pslv_source$(index)=trim$(source$)
66528       let pslv_path$(index)=trim$(path$)
66530       let pslv_text$(index)=trim$(text$)
66532       let pslv_color$(index)=trim$(color$)
66534       let pslv_refnumber(index)=refnumber
66536     else 
66538       if ~warned then 
66540         let msgbox("We ran out of memory to display all the results we found. Try running your search under BR 4.2 or Higher.","BR Data Structure Limitation","Ok","ERR")
66542         let warned=1
66544       end if 
66546     end if 
66548   fnend 
66550 ! 
66552 ! .! Execute the Search based on PS_Data$ and populate mat PSLV_Source$,mat PSLV_Path$,mat PSLV_Text$,mat PSLV_Color$,mat PSLV_RefNumber
66554   def fnsearch(mat ps_data$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,mwindow)
66556     open #(mwindow:=fngetfilenumber) : "SROW=14,SCOL=20,ROWS=3,COLS=60,BORDER=S",display,outin 
66558     print #mwindow, fields "2,2,CC 58" : "Searching. Please wait..."
66560 ! 
66562     let warned=0
66564     if ps_data$(_ps_screen)(1:1)="^" then 
66566       print #mwindow, fields "2,2,CC 58" : "Searching Screens. Please wait..."
66568       let fnsearchscreen(ps_data$(_ps_searchtext),mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66570     end if 
66572     if ps_data$(_ps_layout)(1:1)="^" then 
66574       print #mwindow, fields "2,2,CC 58" : "Searching File Layouts. Please wait..."
66576       let fnsearchfilelay(ps_data$(_ps_searchtext),mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66578     end if 
66580     if ps_data$(_ps_function)(1:1)="^" then 
66582       print #mwindow, fields "2,2,CC 58" : "Searching Screen Functions. Please wait..."
66584       let fnsearchfunctions(ps_data$(_ps_searchtext),mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66586     end if 
66588     if ps_data$(_ps_helperlib)(1:1)="^" then 
66590       print #mwindow, fields "2,2,CC 58" : "Searching Compiled Helper Libraries. Please wait..."
66592       let fnsearchhelperlibs(ps_data$(_ps_searchtext),mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
66594     end if 
66596     if ps_data$(_ps_path)(1:1)="^" or ps_data$(_ps_usealtext)(1:1)="^" then 
66598       if len(trim$(ps_data$(_ps_folder))) and exists(trim$(ps_data$(_ps_folder))) and trim$(ps_data$(_ps_folder))<>"\" then 
66600         if ps_data$(_ps_includesubs)(1:1)=="^" then 
66602           print #mwindow, fields "2,2,CC 58" : "Searching Folder and Subfolders. Please wait..."
66604         else 
66606           print #mwindow, fields "2,2,CC 58" : "Searching Folder. Please wait..."
66608         end if 
66610         let fnsearchprograms(ps_data$(_ps_searchtext),ps_data$(_ps_folder),ps_data$(_ps_mask),ps_data$(_ps_altext),(ps_data$(_ps_path)(1:1)=="^"),(ps_data$(_ps_usealtext)(1:1)=="^"),(ps_data$(_ps_altextobject)(1:1)=="^"),(ps_data$(_ps_includesubs)(1:1)=="^"),(ps_data$(_ps_searchallext)(1:1)=="^"),mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,mwindow)
66612       end if 
66614     end if 
66616     close #mwindow: 
66618   fnend 
66620 ! 
66622 ! 
66624   dim searchscreenio$(1)*1023, searchscreenio(1)
66626   dim searchscreenfld$(1)*1023, searchscreenfld(1)
66628 ! 
66630   def fnsearchscreen(&text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,i,control,lastscreen$,prefix$)
66632     mat searchscreenio$(udim(mat screenio$))
66634     mat searchscreenio(udim(mat screenio))
66636     mat searchscreenfld$(udim(mat screenfld$))
66638     mat searchscreenfld(udim(mat screenfld))
66640 ! 
66642     restore #fscreenio: 
66644     restore #fscreenfld: 
66646 ! 
66648     do until file(fscreenio)
66650       read #fscreenio, using form$(fscreenio), release: mat searchscreenio$, mat searchscreenio eof ignore
66652       if file(fscreenio)=0 then 
66654         for i=1 to udim(mat searchscreenio$)
66656           if pos(lwrc$(searchscreenio$(i)),lwrc$(trim$(text$))) then 
66658             let fnaddresult(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,"Screen",lwrc$(searchscreenio$(si_screencode))&"\"&lwrc$(screenio_ssubs$(i))&"$",searchscreenio$(i),fnfieldonscreen(i)) ! Match Found
66660           end if 
66662         next i
66664       end if 
66666     loop 
66668     do until file(fscreenfld)
66670       read #fscreenfld, using form$(fscreenfld), release: mat searchscreenfld$, mat searchscreenfld eof ignore
66672       if lastscreen$<>searchscreenfld$(sf_screencode) then 
66674         let control=0
66676         let lastscreen$=searchscreenfld$(sf_screencode)
66678         read #fscreenio, using form$(fscreenio), key=fnkey$(fscreenio,lastscreen$), release: mat searchscreenio$, mat searchscreenio nokey ignore
66680         if file(fscreenio)=0 then 
66682           let fnreadlayoutarrays(searchscreenio$(si_filelay),prefix$)
66684         else 
66686           let prefix$=""
66688         end if 
66690       end if 
66692       let control+=1
66694       if file(fscreenfld)=0 then 
66696         for i=1 to udim(mat searchscreenfld$)
66698           if ((i><sf_fieldname) and pos(lwrc$(searchscreenfld$(i)),lwrc$(trim$(text$)))) or ((i=sf_fieldname) and pos(lwrc$(trim$(prefix$)&trim$(searchscreenfld$(i))),lwrc$(trim$(text$)))) then 
66700             let fnaddresult(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,"Screen",lwrc$(searchscreenfld$(sf_screencode))&"\"&fnfieldhandle$(mat searchscreenfld$,mat searchscreenfld)&"\"&lwrc$(screenfld_ssubs$(i))&"$",searchscreenfld$(i),control) ! Match Found
66702           end if 
66704         next i
66706       end if 
66708     loop 
66710   fnend 
66712 ! 
66714   def fnfieldhandle$*200(mat screenfld$,mat screenfld)
66716     if trim$(screenfld$(sf_controlname))<>"" then 
66718       let fnfieldhandle$=lwrc$(screenfld$(sf_fieldtype))&":"&screenfld$(sf_controlname)
66720     else if trim$(screenfld$(sf_fieldname))<>"" then 
66722       let fnfieldhandle$=lwrc$(screenfld$(sf_fieldtype))&":<"&screenfld$(sf_fieldname)&">"
66724     else if trim$(screenfld$(sf_description))<>"" and lwrc$(screenfld$(sf_fieldtype))=="caption" or lwrc$(screenfld$(sf_fieldtype))=="button" then 
66726       let fnfieldhandle$=lwrc$(screenfld$(sf_fieldtype))&":"""&screenfld$(sf_description)&""""
66728     else if trim$(screenfld$(sf_picture))<>"" and lwrc$(screenfld$(sf_fieldtype))=="p" then 
66730       let fnfieldhandle$=lwrc$(screenfld$(sf_fieldtype))&":"""&screenfld$(sf_picture)&""""
66732     else if trim$(screenfld$(sf_function))<>"" and lwrc$(screenfld$(sf_fieldtype))=="caption" or lwrc$(screenfld$(sf_fieldtype))=="button" then 
66734       let fnfieldhandle$=lwrc$(screenfld$(sf_fieldtype))&":"""&screenfld$(sf_description)&""""
66736     else 
66738       if lwrc$(screenfld$(sf_fieldtype))="listchld" or lwrc$(screenfld$(sf_fieldtype))="listview" then 
66740         let fnfieldhandle$=lwrc$(screenfld$(sf_fieldtype))&":["&str$(screenfld(sf_vposition))&","&str$(screenfld(sf_hposition))&",list "&str$(screenfld(sf_width))&"/"&str$(screenfld(sf_height))&"]"
66742       else 
66744         let fnfieldhandle$=lwrc$(screenfld$(sf_fieldtype))&":["&str$(screenfld(sf_vposition))&","&str$(screenfld(sf_hposition))&","&fncalculatefieldtype$(screenfld$(sf_fieldtype),screenfld$(sf_justify),screenfld(sf_width),screenfld(sf_specwidth),screenfld(sf_height))&"]"
66746       end if 
66748     end if 
66750   fnend 
66752 ! 
66754   dim d1$(1)*36, d2$(1)*36, d3$(1)*36, d4$(1)*36, d5$(1)*255
66756   dim as_reference(1), readfieldonscreen
66758   def fnfieldonscreen(i)
66760     if ~readfieldonscreen then 
66762       let fndefineattributeswindow(mat d1$,mat d2$,mat d3$,mat d4$,mat d5$,mat as_reference)
66764       let readfieldonscreen=1
66766     end if 
66768     let fnfieldonscreen=max(0,srch(mat as_reference,i))
66770   fnend 
66772 ! 
66774   dim stringoftext$*10000
66776   def fnsearchtextfile(filename$*255,&text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,type$*40;path$*255,___,textfile)
66778     open #(textfile:=fngetfilenumber): "name="&filename$, display, input error ignore
66780     if file(textfile)=0 then 
66782       if path$="" then let path$=filename$
66784       do until file(textfile)
66786         linput #textfile: stringoftext$ error ignore
66788         if file(textfile)=0 then 
66790           if pos(lwrc$(stringoftext$),lwrc$(trim$(text$))) then 
66792             let fnshrinkaround(stringoftext$,trim$(text$),150)
66794             let fnaddresult(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,type$,path$,stringoftext$)
66796           end if 
66798         end if 
66800       loop 
66802       close #textfile: 
66804     end if 
66806   fnend 
66808 ! 
66810   def fnshrinkaround(&longstring$,searchstring$*255,size;___,x,q,s,e)
66812     let longstring$=trim$(longstring$)
66814     if len(longstring$)>size then 
66816       let q=int(size/4)
66818       let x=pos(lwrc$(longstring$),lwrc$(searchstring$))
66820       if x>0 then 
66822         let s=max(1,x-q)
66824         let e=min(len(longstring$),s+size-1)
66826         let s=max(1,e-size+1)
66828         let longstring$=longstring$(s:e)
66830       else 
66832         let longstring$=longstring$(1:size)
66834       end if 
66836     end if 
66838   fnend 
66840 ! 
66842   dim filelist$(1)*255
66844   dim searchssubs$(1)*255, searchnsubs$(1)*255
66846   def fnsearchfilelay(&text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index,layoutpath$*255,prefix$,jndex)
66848 ! .   ! Text File Search
66850     let layoutpath$=fnreadlayoutpath$
66852     let fnreadlayouts(mat filelist$)
66854 ! 
66856     for index=1 to udim(mat filelist$)
66858       let fnsearchtextfile(layoutpath$&filelist$(index),text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,"Layout")
66860       let fnreadlayoutarrays(filelist$(index),prefix$,mat searchssubs$,mat searchnsubs$)
66862       for jndex=1 to udim(mat searchssubs$)
66864         if pos(lwrc$(trim$(prefix$)&trim$(searchssubs$(jndex))),lwrc$(trim$(text$))) then 
66866           let fnaddresult(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,"Layout",lwrc$("filelay\"&filelist$(index))&"\"&lwrc$(trim$(prefix$)&trim$(searchssubs$(jndex)))&"$",lwrc$(trim$(prefix$)&trim$(searchssubs$(jndex)))&"$") ! Match Found
66868         end if 
66870       next jndex
66872       for jndex=1 to udim(mat searchnsubs$)
66874         if pos(lwrc$(trim$(prefix$)&trim$(searchnsubs$(jndex))),lwrc$(trim$(text$))) then 
66876           let fnaddresult(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,"Layout",lwrc$("filelay\"&filelist$(index))&"\"&lwrc$(trim$(prefix$)&trim$(searchnsubs$(jndex))),lwrc$(trim$(prefix$)&trim$(searchnsubs$(jndex)))) ! Match Found
66878         end if 
66880       next jndex
66882     next index
66884   fnend 
66886 ! 
66888   def fnsearchfunctions(&text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index)
66890 ! .   ! Text File Search
66892     let fnreadfiles(mat filelist$,setting_functionfolder$,"brs",1,0,1)
66894 ! 
66896     for index=1 to udim(mat filelist$)
66898       let fnsearchtextfile(setting_functionfolder$&filelist$(index),text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,"Function")
66900     next index
66902 ! 
66904     if exists(setting_functionfolder$&"defaults\") then 
66906       let fnreadfiles(mat filelist$,setting_functionfolder$&"defaults\","brs",1,0,1)
66908       for index=1 to udim(mat filelist$)
66910         let fnsearchtextfile(setting_functionfolder$&"defaults\"&filelist$(index),text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,"Function")
66912       next index
66914     end if 
66916   fnend 
66918 ! 
66920   def fnsearchhelperlibs(&text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index)
66922 ! .   ! Text File Search
66924     let fnreadfiles(mat filelist$,"screenio","brs",1,0,1)
66926 ! 
66928     for index=1 to udim(mat filelist$)
66930       let fnsearchtextfile(""&setting_screenfolder$&"\"&filelist$(index),text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,"Helper Library")
66932     next index
66934   fnend 
66936 ! 
66938   def fnnewext(filename$*128;altext$,___,ext$,position)
66940     let position=pos(filename$,".",-1)
66942     if position then let ext$=filename$(position+1:len(filename$))
66944     if lwrc$(trim$(ext$)) = "brs" or lwrc$(trim$(ext$)) = "wbs" or lwrc$(trim$(ext$)) = "br" or lwrc$(trim$(ext$)) = "wb" or lwrc$(trim$(ext$)) = lwrc$(trim$(altext$)) or lwrc$(trim$(ext$)) = "bak" then ! #Select# lwrc$(trim$(Ext$)) #Case# "brs" # "wbs" # "br" # "wb" # lwrc$(trim$(AltExt$)) # "bak"
66946     else ! #Case Else#
66948       if ~pos(lwrc$(filename$),"brlog.txt") and ~pos(lwrc$(filename$),"fileio.log") and ~pos(lwrc$(filename$),"dir.doc") then 
66950         let fnnewext=1
66952       end if 
66954     end if  ! #End Select#
66956   fnend 
66958 ! 
66960   dim tastring$*2000
66962   def fnvalidasciifile(filename$*128;___,filen)
66964     open #(filen:=fngetfilenumber): "name="&filename$&", recl=2000", display, input error SKIPTHISFILE
66966     if file(filen)=0 then 
66968       linput #filen: tastring$ error ignore
66970       if file(filen)=0 then 
66972         let fnvalidasciifile=fnascii(string$)
66974       end if 
66976     end if 
66978     close #filen: 
66980 SKIPTHISFILE: ! Jumps here if we can't even open it
66982   fnend 
66984 ! 
66986   def fnascii(&string$;___,char,index)
66988     let fnascii=1
66990     do while (index:=index+1)<=len(string$)
66992       let char=ord(string$(index:index))
66994       if (char<32 and char><10 and char><13 and char><26) or char>=127 then 
66996         let fnascii=0
66998         exit do 
67000       end if 
67002     loop 
67004   fnend 
67006 ! 
67008   dim directory$*9999
67010   def fnbuildfolderlist(mat list$;folder$,___,folderfile,lastchar)
67012     let folder$=br_filename$(os_filename$(".")) ! Current dir first
67014     let lastchar=len(folder$)
67016     if folder$(lastchar:lastchar)="\" then let folder$=folder$(1:lastchar-1)
67018     mat list$(1)
67020     let list$(1)=folder$ ! Current dir first
67022     if ~exists("dirlist.sch") then execute "system -M dir """&os_filename$(folder$)&""" /s /b /ad >dirlist.sch"
67024     open #(folderfile:=fngetfilenumber): "name=dirlist.sch, recl=2000", display, input 
67026     do until file(folderfile)
67028       linput #folderfile : directory$ eof ignore
67030       if file(folderfile)=0 then 
67032         if br_filename$(trim$(directory$))(1:1)<>":" and pos(br_filename$(trim$(directory$))," ")=0 and pos(directory$,".svn")=0 and pos(directory$,"filelay\version")=0 then 
67034           mat list$(udim(mat list$)+1)
67036           let list$(udim(mat list$))=br_filename$(directory$)
67038         end if 
67040       end if 
67042     loop 
67044 ! 
67046     close #folderfile: 
67048 ! .   ! execute "*free dirlist.sch"
67050   fnend 
67052 ! 
67054 ! 
67056   dim alreadysearched$(1)*255
67058   dim warnedaboutfolder
67060   dim searchfolders$(1)*255
67062   dim filefolderlist$(1)*255
67064 ! 
67066   dim allextfiles$(1)*255
67068   def fnsearchprograms(&text$,&folder$,&mask$,&altext$,usestandard,usealtext,altextobject,includesubfolders,includeallext,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;mwindow,___,index,waitcount,compilenumber,waittime,saidsomething,procfile,dirindex,folderlistindex,testaltext$,numberoffiles)
67070     if includesubfolders then 
67072 ! .      ! Build a DirList$
67074       let fnbuildfolderlist(mat searchfolders$,folder$)
67076     else 
67078       mat searchfolders$(1)
67080       let searchfolders$(1)=folder$
67082     end if 
67084 ! 
67086     if mwindow then print #mwindow, fields "2,2,CC 58" : "Searching Folder: Finding Files. Please wait..."
67088 ! 
67090 ! .   ! Text File Search
67092     mat filelist$(0) : mat filefolderlist$(0)
67094     for dirindex=1 to udim(mat searchfolders$)
67096       if usestandard then 
67098         let fnreadfiles(mat filelist$,trim$(searchfolders$(dirindex)),"brs",0,0,1,mask$)
67100         let fnreadfiles(mat filelist$,trim$(searchfolders$(dirindex)),"wbs",0,0,1,mask$)
67102       end if 
67104       if usealtext and ~altextobject then 
67106         let fnreadfiles(mat filelist$,trim$(searchfolders$(dirindex)),lwrc$(altext$),0,0,1,mask$)
67108       end if 
67110 ! 
67112       if includeallext then ! Then check for all extensions and add the others in here if they're valid text files.
67114         if usealtext then let testaltext$=altext$ else let testaltext$=""
67116         mat allextfiles$(0)
67118         let fnreadfiles(mat allextfiles$,trim$(searchfolders$(dirindex)),"",0,0,1,mask$)
67120         for index=1 to udim(mat allextfiles$)
67122           if fnnewext(allextfiles$(index),testaltext$) and fnvalidasciifile(searchfolders$(dirindex)&"\"&allextfiles$(index)) then 
67124             mat filelist$(udim(mat filelist$)+1)
67126             let filelist$(udim(mat filelist$))=allextfiles$(index)
67128           end if 
67130         next index
67132       end if 
67134 ! 
67136 ! .      ! Record which folder to use later.
67138       let folderindex=udim(mat filefolderlist$)+1
67140       mat filefolderlist$(udim(mat filelist$))
67142 ! 
67144       if udim(mat filefolderlist$)>=folderindex then 
67146         mat filefolderlist$(folderindex:udim(mat filefolderlist$))=(searchfolders$(dirindex))
67148       end if 
67150     next dirindex
67152 ! 
67154     mat alreadysearched$(udim(mat filelist$))
67156 ! 
67158     if mwindow then print #mwindow, fields "2,2,CC 58" : "Searching Folder: Searching Ascii Files. Please wait..."
67160     for index=1 to udim(mat filelist$)
67162       let alreadysearched$(index)=lwrc$(trim$(filefolderlist$(index)&"\"&filelist$(index)))
67164       let fnsearchtextfile(trim$(filefolderlist$(index))&"\"&filelist$(index),text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,"Program")
67166     next index
67168 ! 
67170     if mwindow then print #mwindow, fields "2,2,CC 58" : "Searching Folder: Finding BR Object Files. Please wait..."
67172 ! 
67174 ! .   ! Also search for .br and .wb files
67176     if folder$(1:1)=":" then 
67178       if ~warnedaboutfolder then 
67180         let msgbox("Warning: It is not possible to search for BR Object files outside of your BR path. Only Source files will be searched.","Warning:")
67182         let warnedaboutfolder=1
67184       end if 
67186     else 
67188       mat filelist$(0) : mat filefolderlist$(0)
67190       for dirindex=1 to udim(mat searchfolders$)
67192         if usestandard then 
67194           let fnreadfiles(mat filelist$,trim$(searchfolders$(dirindex)),"br ",0,0,1,mask$)
67196           let fnreadfiles(mat filelist$,trim$(searchfolders$(dirindex)),"wb ",0,0,1,mask$)
67198         end if 
67200         if usealtext and altextobject then 
67202           let fnreadfiles(mat filelist$,trim$(searchfolders$(dirindex)),lwrc$(altext$),0,0,1,mask$)
67204         end if 
67206 ! 
67208         let folderindex=udim(mat filefolderlist$)+1
67210         mat filefolderlist$(udim(mat filelist$))
67212 ! 
67214         if udim(mat filefolderlist$)>=folderindex then 
67216           mat filefolderlist$(folderindex:udim(mat filefolderlist$))=(searchfolders$(dirindex))
67218         end if 
67220       next dirindex
67222 ! 
67224       if mwindow then print #mwindow, fields "2,2,CC 58" : "Searching Folder: Building Decompile Proc. Please wait..."
67226       for index=1 to udim(mat filelist$)
67228         if srch(mat alreadysearched$,lwrc$(trim$(filefolderlist$(index)&"\"&filelist$(index)))&"s")<1 and srch(mat alreadysearched$,lwrc$(trim$(filefolderlist$(index)&"\"&srep$(filelist$(index),".wb",".brs"))))<1 then 
67230           if lwrc$(trim$(filelist$(index)))<>"screenio.br" then 
67232             if ~procfile then let procfile=fnopendecompileproc(compilenumber)
67234             let fnbuilddecompileproc(filelist$(index),filefolderlist$(index),procfile)
67236             let numberoffiles+=1
67238           end if 
67240         end if 
67242       next index
67244       if mwindow then print #mwindow, fields "2,2,CC 58" : "Searching Folder: Decompiling BR Programs. Please wait..."
67246       if procfile then let fnclosedecompileproc(procfile,compilenumber)
67248 ! 
67250       let waitcount=timer
67252       let waittime=numberoffiles*.3 ! seconds
67254       let saidsomething=0
67256       do while exists("compile"&str$(compilenumber)&".$$$")
67258         let sleep(.1)
67260         if timer-waitcount>waittime then 
67262           if (2==msgbox("We've been waiting a long time. There were "&str$(numberoffiles)&" files to decompile. Could something have gotten stuck, perhaps? Do you want to wait longer?","Wait Longer?","Yn")) then 
67264             let waitcount=timer
67266             let saidsomething=1
67268           end if 
67270         end if 
67272       loop until timer-waitcount>waittime
67274 ! 
67276       if exists("compile"&str$(compilenumber)&".$$$") then 
67278         let msgbox("Search Failed for .BR files","Error")
67280         execute "free compile"&str$(compilenumber)&".$$$" error ignore
67282         if exists("compile"&str$(compilenumber)&".$$$") then let compilenumber+=1
67284       else if saidsomething then 
67286         let msgbox("We were able to recover the search.")
67288       end if 
67290 ! 
67292       if mwindow then print #mwindow, fields "2,2,CC 58" : "Searching Folder: Searching BR Programs. Please wait..."
67294       for index=1 to udim(mat filelist$)
67296         if exists("temp_"&lwrc$(trim$(filelist$(index)))&".brs") then 
67298           let fnsearchtextfile("temp_"&lwrc$(trim$(filelist$(index)))&".brs",text$,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber,"Program",trim$(filefolderlist$(index))&"\"&filelist$(index))
67300           execute "free ""temp_"&lwrc$(trim$(filelist$(index)))&".brs"""
67302         end if 
67304       next index
67306     end if 
67308   fnend 
67310 ! 
67312   def fnopendecompileproc(&compilenumber;___,filenumber)
67314     open #(filenumber:=fngetfilenumber): "name=compile"&str$(compilenumber)&".$$$, replace", display, output error TRYNEXTNUMBER
67316     print #filenumber: "proc noecho"
67318     let fnopendecompileproc=filenumber
67320   fnend 
67322 ! 
67324 TRYNEXTNUMBER: ! Try a new number
67326   let compilenumber+=1
67328   retry 
67330 ! 
67332   def fnclosedecompileproc(filenumber,compilenumber)
67334     print #filenumber: "system"
67336     close #filenumber: 
67338 ! 
67340 ! .   ! Extract Source Code
67342     execute "system -C -M "&fngetbrexe$&" -"&fngetwbcfg$&" proc compile"&str$(compilenumber)&".$$$"
67344   fnend 
67346 ! 
67348   def fnbuilddecompileproc(filename$*255,folder$*255,filenumber)
67350     print #filenumber: "load """&trim$(folder$)&"\"&trim$(filename$)&""""
67352     print #filenumber: "list >""temp_"&lwrc$(trim$(filename$))&".brs"""
67354   fnend 
67356 ! 
67358   def fnprintlisttofile(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,filenumber,filename$*255)
67360 ! .   ! Browse for a file
67362     open #(filenumber:=fngetfilenumber): "name=save:.\*.txt, recl=1024,replace", display, output error ignore
67364     if filenumber and file(filenumber)>-1 then 
67366       let fnprintlist(filenumber,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
67368       let filename$=file$(filenumber)
67370       close #filenumber: 
67372       execute "system -M -C start ""Launching"" """&os_filename$(filename$)&""""
67374     end if 
67376   fnend 
67378 ! 
67380   def fnprintlisttoprinter(mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
67382 ! .   ! Open the printer
67384     let fnprintersys
67386     open #255: "name=preview:/default, recl=1024", display, output 
67388     print #255: "[LANDSCAPE][A4PAPER][SETFONT(SystemPC)][LPI(8)][CPI(16)]";
67390     let fnprintlist(255, mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber)
67392     close #255: 
67394   fnend 
67396 ! 
67398   def fnprintlist(filenumber,mat pslv_source$,mat pslv_path$,mat pslv_text$,mat pslv_color$,mat pslv_refnumber;___,index,sourcelen,pathlen,textlen,printer)
67400     if filenumber=255 then let printer=1
67402     let sourcelen=7
67404     let pathlen=5
67406     let textlen=5
67408     for index=1 to udim(mat pslv_source$)
67410       let sourcelen=max(sourcelen,len(pslv_source$(index)))
67412       let pathlen=max(pathlen,len(pslv_path$(index)))
67414       let textlen=max(textlen,len(pslv_text$(index)))
67416     next index
67418     print #filenumber: "PowerSearch: """&trim$(ps_data$(_ps_searchtext))&""" Folder: """&trim$(ps_data$(_ps_folder))&""" "&date$("Day, Month dd, ccyy")&", "&time$
67420     print #filenumber: 
67422     print #filenumber: srep$(rpad$(" Source",sourcelen)," ","_")&" "&srep$(rpad$(" Path",pathlen)," ","_")&" "&srep$(rpad$(" Text",textlen)," ","_")
67424     for index=1 to udim(mat pslv_source$)
67426       if printer then 
67428         if pslv_color$(index)="#FFFFFF" then 
67430           print #filenumber: "[SETCOLOR(#000000)]";
67432         else 
67434           print #filenumber: "[SETCOLOR("&pslv_color$(index)&")]";
67436         end if 
67438       end if 
67440       print #filenumber: rpad$(trim$(pslv_source$(index)),sourcelen)&" "&rpad$(trim$(pslv_path$(index)),pathlen)&" "&rpad$(trim$(pslv_text$(index)),textlen);
67442       if ~printer then 
67444         print #filenumber: " "&fncolorname$(pslv_color$(index))
67446       else 
67448         print #filenumber: 
67450       end if 
67452     next index
67454   fnend 
67456 ! 
67458   def fnprintlisttoprintersimple(mat data$)
67460 ! .   ! Open the printer
67462     open #255: "name=preview:/default, recl=1024", display, output 
67464     let fnprintlistsimple(255, mat data$)
67466     close #255: 
67468   fnend 
67470 ! 
67472   def fnprintlistsimple(filenumber,mat data$;___,index)
67474     for index=1 to udim(mat data$)
67476       print #filenumber: data$(index)
67478     next index
67480   fnend 
67482 ! 
67484 ! 
67486   def fncolorname$(color$)
67488     if uprc$(trim$(color$(2:7))) = "FFFFFF" then ! #Select# uprc$(trim$(color$(2:7))) #Case# "FFFFFF"
67490       let fncolorname$="White"
67492     else if uprc$(trim$(color$(2:7))) = "FFFF00" then ! #Case# "FFFF00"
67494       let fncolorname$="Yellow"
67496     else if uprc$(trim$(color$(2:7))) = "00FF00" then ! #Case# "00FF00"
67498       let fncolorname$="Green"
67500     end if  ! #End select#
67502   fnend 
67504 ! 
67506   dim printersysset
67508   def fnprintersys ! Activate Printer.Sys commands
67510     if ~printersysset then 
67512       execute "config PRINTER TYPE NWP select WIN:"
67514       execute "config PRINTER TYPE NWP select PREVIEW:"
67516 ! 
67518       execute 'config PRINTER NWP [SETCOLOR(CCCCCCC)], "\Ecolor=''CCCCCCC''"'
67520       execute 'config PRINTER NWP [CPI(NNN)], "\E(sNNNH"'
67522       execute 'config PRINTER NWP [LPI(NNN)], "\E&lNNND"'
67524       execute 'config PRINTER NWP [SETFONT(FontName)], "\Efont=''FontName''"'
67526       execute 'config PRINTER NWP [A4PAPER], "\E&l26A" ! 210mm x 297mm'
67528       execute 'config PRINTER NWP [LANDSCAPE], "\E&l1O"'
67530       let printersysset=1
67532     end if 
67534   fnend 
67536 ! 
68000 !  #AutoNumber# 68000,2
68002 !  =================================================
68004 !  ================= Code Explorer =================
68006 !  =================================================
68008 ! 
68010   dim celv_spec$
68012   dim celv_cap$(2)*30,celv_width(2),celv_spec$(2)
68014   dim ce_capspec$(2)*30, ce_caption$(2)*30
68016 ! 
68018   dim celv_path$(1)*255
68020   dim celv_location$(1)*255
68022   dim celv_selected(1)
68024 ! 
68026   def fncodeexplore(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,function,codeexplore,rowcount,done)
68028 ! .   ! Initialize PowerSearch Results Listview
68030     let celv_spec$="1,1,LIST 22/50"
68032 ! 
68034 ! .   ! Initialize PowerSearch Results Listview Columns
68036     let celv_cap$(1)="Function"
68038     let celv_width(1)=20
68040     let celv_spec$(1)="C 255"
68042 ! 
68044     let celv_cap$(2)="Location"
68046     let celv_width(2)=10
68048     let celv_spec$(2)="C 255"
68050 ! 
68052     mat celv_path$(0)
68054     mat celv_location$(0)
68056 ! 
68058 ! .   ! Initialize Code Explorer Buttons and Caption Specs
68060     mat ce_capspec$(2) : mat ce_caption$(2)
68062     let ce_capspec$(1)="23,3,10/CC 17,/W:W,B36" ! Jump To Item
68064     let ce_capspec$(2)="23,14,15/CC 26,/W:W,B37" ! Affected Screen List
68066 ! 
68068     let ce_caption$(1)="Launch"
68070     let ce_caption$(2)="Affected Screens"
68072 ! 
68074     open #(codeexplore:=fngetfilenumber): "srow=4,scol=30,rows=23,cols=50,Border=s,Caption=Code Explorer", display, outin 
68076 ! 
68078     print #codeexplore, fields mat ce_capspec$ : mat ce_caption$
68080     print #codeexplore, fields celv_spec$&",HEADERS" : (mat celv_cap$, mat celv_width, mat celv_spec$)
68082 ! 
68084     let fnpopulatecodeexplore(mat celv_path$,mat celv_location$,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
68086 ! 
68088     if udim(mat celv_path$) then 
68090 ! .      ! Predraw listview if any data belongs in it
68092       print #codeexplore, fields celv_spec$&",=" : (mat celv_path$,mat celv_location$)
68094       do 
68096         input #codeexplore, fields celv_spec$&",ROWCNT,SEL" : rowcount
68098         mat celv_selected(rowcount)
68100         input #codeexplore, fields celv_spec$&",ROWSUB,SEL,NOWAIT" : mat celv_selected
68102 ! 
68104         let function=fkey
68106 ! 
68108         if function = 36 then ! #Select# Function #Case# 36
68110 ! .            ! Jump To Item
68112           let fncejumpto(mat celv_selected,mat celv_path$)
68114 ! 
68116         else if function = 37 then ! #Case# 37
68118 ! .            ! Affected Screen List
68120           let done=fnceaffectedscreenlist(mat celv_selected,mat celv_path$)
68122 ! 
68124         end if  ! #End Select#
68126 ! 
68128       loop until function=99 or function=98 or function=93 or ((function>1100) and (function<1699)) or function=19 or fkey=93 or done
68130     else 
68132       let msgbox("No functions are defined in the current screen.","Nothing to display")
68134     end if 
68136     if codeexplore and file(codeexplore)>-1 then close #codeexplore: 
68138   fnend 
68140 ! 
68142   def fnpopulatecodeexplore(mat path$,mat location$,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,loc$*255)
68144     for index=si_enterfn to si_exitfn
68146       if len(trim$(screenio$(index))) then 
68148         let fnaddceresult(trim$(screenio$(index)),mat path$,mat location$,fneventname$(-1,index))
68150       end if 
68152     next index
68154 ! 
68156     for index=1 to udim(mat controlname$)
68158       if len(trim$(function$(index))) then 
68160         let fnaddceresult(trim$(function$(index)),mat path$,mat location$,fneventname$(index,1,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
68162       end if 
68164       if len(trim$(cnvrtin$(index))) then 
68166         let fnaddceresult(trim$(function$(index)),mat path$,mat location$,fneventname$(index,2,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
68168       end if 
68170       if len(trim$(cnvrtout$(index))) then 
68172         let fnaddceresult(trim$(function$(index)),mat path$,mat location$,fneventname$(index,3,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines))
68174       end if 
68176     next index
68178 ! 
68180     if exists(setting_functionfolder$) then 
68182       if exists(setting_functionfolder$&"defaults\") then 
68184         if exists(setting_functionfolder$&"defaults\enter.brs") then 
68186           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\enter.brs"),mat path$,mat location$,"Default Screen Enter Event")
68188         end if 
68190         if exists(setting_functionfolder$&"defaults\init.brs") then 
68192           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\init.brs"),mat path$,mat location$,"Default Screen Init Event")
68194         end if 
68196         if exists(setting_functionfolder$&"defaults\read.brs") then 
68198           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\read.brs"),mat path$,mat location$,"Default Screen Read Event")
68200         end if 
68202         if exists(setting_functionfolder$&"defaults\load.brs") then 
68204           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\load.brs"),mat path$,mat location$,"Default Screen Load Event")
68206         end if 
68208         if exists(setting_functionfolder$&"defaults\write.brs") then 
68210           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\write.brs"),mat path$,mat location$,"Default Screen Write Event")
68212         end if 
68214         if exists(setting_functionfolder$&"defaults\wait.brs") then 
68216           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\wait.brs"),mat path$,mat location$,"Default Screen Wait Event")
68218         end if 
68220         if exists(setting_functionfolder$&"defaults\locked.brs") then 
68222           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\locked.brs"),mat path$,mat location$,"Default Screen Locked Event")
68224         end if 
68226         if exists(setting_functionfolder$&"defaults\merge.brs") then 
68228           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\merge.brs"),mat path$,mat location$,"Default Screen Merge Event")
68230         end if 
68232         if exists(setting_functionfolder$&"defaults\mainloop.brs") then 
68234           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\mainloop.brs"),mat path$,mat location$,"Default Screen Mainloop Event")
68236         end if 
68238         if exists(setting_functionfolder$&"defaults\nokey.brs") then 
68240           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\nokey.brs"),mat path$,mat location$,"Default Screen Nokey Event")
68242         end if 
68244         if exists(setting_functionfolder$&"defaults\prelist.brs") then 
68246           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\prelist.brs"),mat path$,mat location$,"Default Screen PreList Event")
68248         end if 
68250         if exists(setting_functionfolder$&"defaults\postlist.brs") then 
68252           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\postlist.brs"),mat path$,mat location$,"Default Screen PostList Event")
68254         end if 
68256         if exists(setting_functionfolder$&"defaults\exit.brs") then 
68258           let fnaddceresult(fnfunctionstring$(setting_functionfolder$&"defaults\exit.brs"),mat path$,mat location$,"Default Screen Exit Event")
68260         end if 
68262       end if 
68264     end if 
68266 ! 
68268   fnend 
68270   def fneventname$*255(control,type;mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
68272     if control<0 then 
68274 ! .      ! Window Level Event, no Control Given
68276       let fneventname$="Screen "&fnwindowfunction$(type)&" Event"
68278     else 
68280 ! .      ! Control Given. Check its type to get Verb.
68282 ! .      !  Pull Description out of its name or whatever is available.
68284       if len(trim$(controlname$(control))) then 
68286         let fneventname$=controlname$(control)&" control's "&fneventverb$(fieldtype$(control),type)&" Event"
68288       else if len(trim$(fieldname$(control))) then 
68290         let fneventname$=fieldname$(control)&" field's "&fneventverb$(fieldtype$(control),type)&" Event"
68292       else 
68294         let fneventname$="Control "&str$(control)&"'s "&fneventverb$(fieldtype$(control),type)&" Event"
68296       end if 
68298     end if 
68300   fnend 
68302 ! 
68304   def fnwindowfunction$*64(type)
68306     if type = si_enterfn then ! #Select# Type #Case# si_ENTERFN
68308       let fnwindowfunction$="Enter"
68310     else if type = si_initfn then ! #Case# si_INITFN
68312       let fnwindowfunction$="Initialize"
68314     else if type = si_readfn then ! #Case# si_READFN
68316       let fnwindowfunction$="Read"
68318     else if type = si_loadfn then ! #Case# si_LOADFN
68320       let fnwindowfunction$="Load"
68322     else if type = si_writefn then ! #Case# si_WRITEFN
68324       let fnwindowfunction$="Write"
68326     else if type = si_waitfn then ! #Case# si_WAITFN
68328       let fnwindowfunction$="Wait"
68330     else if type = si_lockedfn then ! #Case# si_LOCKEDFN
68332       let fnwindowfunction$="Record Locked"
68334     else if type = si_mergefn then ! #Case# si_MERGEFN
68336       let fnwindowfunction$="Merge Function"
68338     else if type = si_loopfn then ! #Case# si_LOOPFN
68340       let fnwindowfunction$="Main Loop"
68342     else if type = si_nokeyfn then ! #Case# si_NOKEYFN
68344       let fnwindowfunction$="Nokey"
68346     else if type = si_prelistviewfn then ! #Case# si_PRELISTVIEWFN
68348       let fnwindowfunction$="Listview Prepopulate"
68350     else if type = si_postlistviewfn then ! #Case# si_POSTLISTVIEWFN
68352       let fnwindowfunction$="Listview Postpopulate"
68354     else if type = si_exitfn then ! #Case# si_EXITFN
68356       let fnwindowfunction$="Exit"
68358     end if  ! #End Select#
68360   fnend 
68362 ! 
68364   def fneventverb$*64(type$*255,type)
68366 ! .   ! Check Control Type for Basic Verb
68368     if lwrc$(trim$(type$)) = "button" or lwrc$(trim$(type$)) = "p" or lwrc$(trim$(type$)) = "caption" or lwrc$(trim$(type$)) = "screen" then ! #Select# lwrc$(trim$(Type$)) #Case# "button" # "p" # "caption" # "screen"
68370       let fneventverb$="Click"
68372     else if lwrc$(trim$(type$)) = "c" or lwrc$(trim$(type$)) = "search" or lwrc$(trim$(type$)) = "filter" then ! #Case# "c" # "search" # "filter"
68374       let fneventverb$="Validate"
68376     else if lwrc$(trim$(type$)) = "listview" then ! #Case# "listview"
68378       let fneventverb$="Filter"
68380     end if  ! #End Select#
68382 ! 
68384 ! .   ! Apply Type Override
68386     if type = 2 then ! #Select# Type #Case# 2
68388       let fneventverb$="ConvertIn"
68390     else if type = 3 then ! #Case# 3
68392       let fneventverb$="ConvertOut"
68394     end if  ! #End Select#
68396   fnend 
68398 ! 
68400   def fnaddceresult(function$*255,mat path$;mat location$,loc$*255,___,index)
68402     let index=udim(mat path$)+1
68404 ! 
68406     mat path$(index)
68408     mat location$(index)
68410 ! 
68412     let path$(index)=trim$(function$)
68414     let location$(index)=trim$(loc$)
68416   fnend 
68418 ! 
68420 ! .! Jump To Item
68422   def fncejumpto(mat selected,mat path$;___,index,doneascreen,doneahelperlib,screen$)
68424     for index=1 to udim(mat selected)
68426       if trim$(path$(selected(index)))(1:1)="{" then 
68428 ! .         ! if this item is a function, launch it in editor
68430         let sleep(.1)
68432         execute "system -C -M start "&os_filename$(fncustomfilenameof$(trim$(path$(selected(index))))) error ignore
68434       end if 
68436     next index
68438   fnend 
68440 ! 
68442 ! 
68444   def fnceaffectedscreenlist(mat selected,mat path$;___,index,window,afselcount,function,savescreencode$,done,filename$*255)
68446 ! .   ! Build list of affected screens
68448     mat affectedscreens$(0)
68450     let fncefindaffectedscreens(mat affectedscreens$,mat selected,mat path$)
68452 ! 
68454     if udim(mat affectedscreens$)=0 then 
68456       let msgbox("No screens are affected.","Nothing Found")
68458     else 
68460 ! .      ! Define Listview
68462       let as_caption$(1)="Affected Screens"
68464       let as_width(1)=30
68466       let as_spec$(1)="CC 18"
68468 ! 
68470 ! .      ! Open Window
68472       open #(window:=fngetfilenumber) : "srow=5,scol=30,rows=15,cols=30,border=s",display,outin 
68474 ! 
68476 ! .      ! Paint Window
68478       print #window, fields "1,1,LIST 14/30,HEADERS" : (mat as_caption$,mat as_width,mat as_spec$)
68480       print #window, fields "1,1,LIST 14/30,=" : (mat affectedscreens$)
68482       print #window, fields "15,1,14/CC 16,/W:W,B30;15,16,CC 14,/W:W,B31" : "Recompile Screen","Load Screen"
68484 ! 
68486       do 
68488         input #window, fields "1,1,LIST 14/30,ROWCNT,SEL" : afselcount
68490         let function=fkey
68492         mat af_selection(afselcount)
68494         input #window, fields "1,1,LIST 14/30,ROWSUB,SEL,NOWAIT" : mat af_selection
68496 ! 
68498         if function=30 then ! Recompile Selected Screens
68500           let savescreencode$=static_screenname$
68502 ! 
68504           if exists("compile[SESSION].$$$") then 
68506             execute "free compile[SESSION].$$$"
68508           end if 
68510 ! 
68512           let failedlist$=""
68514           for index=1 to udim(mat af_selection)
68516             if fnreadscreen(affectedscreens$(af_selection(index)),mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
68518               let fncompilehelperlibrary(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,filename$)
68520               let fnwaitforfile(failedlist$,affectedscreens$(af_selection(index)),filename$)
68522             end if 
68524           next index
68526 ! 
68528           if len(failedlist$) then 
68530             let msgbox("The following screens took a long time and may have had errors:"&chr$(13)&chr$(13)&failedlist$(3:9999))
68532           end if 
68534 ! 
68536           let fnreadscreen(savescreencode$,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
68538         end if 
68540 ! 
68542 ! 
68544         if function=31 then ! Load Screen
68546           let fnlaunchscreen(trim$(affectedscreens$(af_selection(1))))
68548 ! .         !   if Fnoktoproceed(Mat Screenio$,Mat Screenio,mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$, mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat Multiselect, mat UserData$, mat GridLines) then
68550 ! .         !      if Fnreadscreen(AffectedScreens$(AF_Selection(1)),Mat Screenio$,Mat Screenio,mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$, mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat Multiselect, mat UserData$, mat GridLines) then
68552 ! .         !         let fnRedrawScreen(mat ScreenIO$,mat ScreenIO,mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$, mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat Multiselect, mat UserData$, mat GridLines,1)
68554 ! .         !
68556 ! .         !         let Done=1
68558 ! .         !      else 
68560 ! .         !         let Msgbox("Error Reading Screen - Screen not  found","Error","OK","ERR")
68562 ! .         !      end if
68564 ! .         !   end if
68566         end if 
68568       loop until function=99 or function=93 or fkey=93 or done
68570       if function=93 or fkey=93 or done then let fnceaffectedscreenlist=1
68572 ! 
68574       if window and file(window)>-1 then close #window: 
68576     end if 
68578   fnend 
68580 ! 
68582   def fncefindaffectedscreens(mat affectedscreens$,mat selected,mat path$;___,function$*255,index,jndex)
68584     for index=1 to udim(mat selected)
68586 ! 
68588 ! .      ! If its a function, search for it in all the screens
68590       let function$=path$(selected(index))
68592 ! 
68594       mat searchscreenio$(udim(mat screenio$))
68596       mat searchscreenio(udim(mat screenio))
68598       mat searchscreenfld$(udim(mat screenfld$))
68600       mat searchscreenfld(udim(mat screenfld))
68602 ! 
68604       restore #fscreenio: 
68606       restore #fscreenfld: 
68608 ! 
68610       do until file(fscreenio)
68612         read #fscreenio, using form$(fscreenio), release: mat searchscreenio$, mat searchscreenio eof ignore
68614         if file(fscreenio)=0 then 
68616           for jndex=si_enterfn to si_exitfn
68618             if lwrc$(trim$(searchscreenio$(jndex)))=lwrc$(trim$(function$)) then 
68620               let fnaddafscreen(searchscreenio$(si_screencode),mat affectedscreens$)
68622             end if 
68624           next jndex
68626         end if 
68628       loop 
68630       do until file(fscreenfld)
68632         read #fscreenfld, using form$(fscreenfld), release: mat searchscreenfld$, mat searchscreenfld eof ignore
68634         if file(fscreenfld)=0 then 
68636           if lwrc$(trim$(searchscreenfld$(sf_function)))=lwrc$(trim$(function$)) then 
68638             let fnaddafscreen(searchscreenfld$(sf_screencode),mat affectedscreens$)
68640           end if 
68642           if lwrc$(trim$(searchscreenfld$(sf_cnvrtin)))=lwrc$(trim$(function$)) then 
68644             let fnaddafscreen(searchscreenfld$(sf_screencode),mat affectedscreens$)
68646           end if 
68648           if lwrc$(trim$(searchscreenfld$(sf_cnvrtout)))=lwrc$(trim$(function$)) then 
68650             let fnaddafscreen(searchscreenfld$(sf_screencode),mat affectedscreens$)
68652           end if 
68654         end if 
68656       loop 
68658 ! 
68660     next index
68662   fnend 
68664 ! 
68666 ! 
68668   dim of_selected(1)
68670   dim of_functions$(1)*255
68672   dim of_function$*255
68674   def fnpopulateorphanedfunctions(mat functions$;___,index,jndex)
68676 ! .   ! Search functions folder and grab all the functions
68678     let fnreadavailablefunctions(mat functions$)
68680 ! 
68682 ! .   ! Add the {}s
68684     for index=1 to udim(mat functions$)
68686       if functions$(index)(1:1)<>"{" then let functions$(index)(1:0)="{"
68688       if functions$(index)(len(functions$(index)):len(functions$(index)))<>"}" then let functions$(index)=functions$(index)&"}"
68690     next index
68692 ! 
68694 ! .   ! Duplicate that list.
68696     mat of_functions$(udim(mat functions$))=functions$
68698 ! 
68700 ! .   !  Loop through the duplicated list and delete any functions that are NOT used, in the end you'll have a list of all functions that ARE used
68702     let index=0
68704     do while (index+=1)<=udim(mat functions$)
68706       mat affectedscreens$(0)
68708       let of_selected(1)=index
68710       let fncefindaffectedscreens(mat affectedscreens$,mat of_selected,mat functions$)
68712 ! 
68714       if udim(mat affectedscreens$)=0 then 
68716 ! .         ! If it is not used, remove it from the list
68718         for jndex=index to udim(mat functions$)-1
68720           let functions$(jndex)=functions$(jndex+1)
68722         next jndex
68724         mat functions$(udim(mat functions$)-1)
68726         let index-=1 ! Go back down one because we removed one.
68728       end if 
68730     loop 
68732 ! 
68734 ! .   !  Go through those functions with a parser written off of the Function Loading technology that searches for any functions that are #included. if they're not there, add them to the list.
68736     let index=0
68738     do while (index+=1)<=udim(mat functions$)
68740       let of_function$=functions$(index)
68742       let fnfindincludes(of_function$,mat functions$)
68744     loop 
68746 ! 
68748 ! .   ! Go through the first list and delete any found in the second list, what you're left with is just the ones that are orphaned.
68750     let index=0
68752     do while (index+=1)<=udim(mat of_functions$)
68754       if srch(mat functions$,of_functions$(index))>0 then 
68756 ! .         ! remove it from the list
68758         for jndex=index to udim(mat of_functions$)-1
68760           let of_functions$(jndex)=of_functions$(jndex+1)
68762         next jndex
68764         mat of_functions$(udim(mat of_functions$)-1)
68766         let index-=1 ! Go back down one because we removed one.
68768       end if 
68770     loop 
68772 ! 
68774     mat functions$(udim(mat of_functions$))=of_functions$ ! Copy it back to return it
68776   fnend 
68778 ! 
68780   dim of_function$(1)*255
68782   def fnorphanedfunctions(;___,function,orphanedfunctions,of_selected,loadmessage)
68784 ! .   ! Initialize PowerSearch Results Listview
68786     let celv_spec$="1,1,LIST 22/30"
68788 ! 
68790 ! .   ! Initialize PowerSearch Results Listview Columns
68792     let celv_cap$(1)="Function"
68794     let celv_width(1)=25
68796     let celv_spec$(1)="C 255"
68798 ! 
68800     mat of_function$(0)
68802 ! 
68804 ! .   ! ! Initialize PowerSearch Buttons and Caption Specs
68806     mat ce_capspec$(1) : mat ce_caption$(1)
68808     let ce_capspec$(1)="23,11,10/CC 17,/W:W,B36" ! Print List
68810 ! 
68812     let ce_caption$(1)="Print"
68814 ! 
68816     open #(orphanedfunctions:=fngetfilenumber): "srow=4,scol=40,rows=23,cols=30,Border=s", display, outin 
68818 ! 
68820     print #orphanedfunctions, fields mat ce_capspec$ : mat ce_caption$
68822     print #orphanedfunctions, fields celv_spec$&",HEADERS" : (mat celv_cap$, mat celv_width, mat celv_spec$)
68824 ! 
68826     open #(loadmessage:=fngetfilenumber): "srow=13,scol=32,rows=3,cols=46,Border=s", display, outin 
68828     print #loadmessage, fields "2,1,CC 40": "Loading .. Please Wait .."
68830 ! 
68832     let fnpopulateorphanedfunctions(mat of_function$)
68834 ! 
68836     close #loadmessage: 
68838 ! 
68840     if udim(mat of_function$) then 
68842 ! .      ! Predraw listview if any data belongs in it
68844       print #orphanedfunctions, fields celv_spec$&",=" : (mat of_function$)
68846       do 
68848         input #orphanedfunctions, fields celv_spec$&",ROWSUB,SELONE" : of_selected
68850 ! 
68852         let function=fkey
68854 ! 
68856         if function = 36 then ! #Select# Function #Case# 36
68858 ! .            ! Print List
68860           let fnprintlisttoprintersimple(mat of_function$)
68862         end if  ! #End Select#
68864 ! 
68866       loop until function=99 or function=98 or function=93 or ((function>1100) and (function<1699)) or function=19 or fkey=93
68868     else 
68870       let msgbox("No Orphaned Functions found.","Nothing to display")
68872     end if 
68874     if orphanedfunctions and file(orphanedfunctions)>-1 then close #orphanedfunctions: 
68876   fnend 
68878 ! 
68880 ! 
68882 ! 
70000 !  #Autonumber# 70000,20
70020 !  =================================================
70040 !  =============== Screen Io Library ===============
70060 !  =================================================
70080 ! 
70100 FM: !   ***** This Is Where All The Magic Happens
70120 !   def library fnScreen$(ScreenName$;KeyVal$*255,Parent_Key$*255,Srow,Scol,Parent_Window,Display_Only,Dontredolistview,Recordval,Path$*255,Selecting,Mat Passeddata$,Usemyf,Mat Myf$,Mat Myf)
70140 ! .   ! This function contains hidden code that prepares the memory,
70160 ! .   !  then calls fnScreenIO to run your screen.
70220 !   fnend
70240 ! 
70260   def library fnfm$*255(screenname$;keyval$*255,srow,scol,parent_key$*255,parent_window,display_only,dontredolistview,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$*255,selecting,savedontask)
70280 ! .   ! This function contains hidden code that prepares the memory,
70300 ! .   !  then calls fnScreenIO to run your screen.
70420   fnend 
70440 ! 
75000 !  #Autonumber# 75000,2
75002 ! 
75004 SCREENIODIMS: ! 
75006   dim f$(1)*1000,f(1),s$(1)*1000
75008   dim screensubs$(1)*80
75010   dim read_f$(1)*1000,read_f(1)
75012   dim disk_f$(1)*1000,disk_f(1)
75014 ! 
75016   dim save_f$(1)*1000,save_f(1),save_s$(1)*1000
75018 ! 
75020   dim selectedkeys$(1)*255
75022   dim selectedrecords(1)
75024 ! 
75026   dim s_data$(1)*1000,s_data(1),s_spec$(1)*255, s_help$(1)*260, s_old$(1)*1000
75028   dim temp_s_spec$(1)*255
75030   dim old_data(1)
75032 ! 
75034   dim save_s_data$(1)*1000
75036   dim save_s_subs(1)
75038   dim changed_s_data$(1)*1000
75040   dim changed_s_subs(1)
75042   dim other_s_data$(1)*1000
75044   dim other_s_subs(1)
75046   dim otherchanged_s_data$(1)*1000
75048   dim otherchanged_s_subs(1)
75050 ! 
75052   dim returndata$(1)*255
75054   dim controlspec$(1)*128
75056   dim screeniossubs$(1)*40
75058   dim screenionsubs$(1)*40
75060 ! 
75062   dim tempindex
75064   dim tempcontrol
75066   dim tempfield
75068   dim foundalready
75070 ENDSCREENIODIMS: ! 
75072 ! 
75074   dim loadedscreencount
75076   def library fnfunctionbase=fnkeybase
75078   def fnstickybase=fnkeybase+500
75080   def fnkeybase=(fnbase+(200*loadedscreencount))
75082   def fnbase=1500
75084 ! 
75086   dim passedinf$(1)*1023,passedinf(1)
75088   dim anothertempmyf$(1)*1023,anothertempmyf(1)
75090 ! 
75092 ! 
75094   def fnscreenio$*255(routinename$,&exitmode;&key$,row,col,&parentkey$,parentwindow,displayonly,record,&path$,selecting,active,forcethisindex,editing,mat passeddata$,usemyf,mat myf$,mat myf,&savedontask,___,fdatafile,wwindow,function,kp$,prefix$,index,currentfield,currentfield$*50,s_currentfield$*50,screenrows,screencols,turnguibackoff,librarylinkage,oldrows,oldcols,oldwindowcaption$*255,fscreenio,fscreenfld,screenioprefix$,subscriptsalreadyset,currentkey$*255,currentrec,s_currentrec,listviewindex,returnwindownumber,_bordersize,currentrow,s_currentrow,myfread,forceindex,x,forcevisibility,redrawframes,redrawscreens,repopulatecombo,nextrow)
75096 ! 
75098     mat f(0)=(0)
75100     mat f$(0)=("")
75102     mat s$(0)=("")
75104 ! 
75106     let exitmode=0
75108 ! 
75110     let fnopenscreenfiles(mat screenio$,mat screenio)
75112     let fnreadlayoutarrays("screenio",screenioprefix$,mat screeniossubs$,mat screenionsubs$)
75114 ! 
75116 !     If Exists(Lwrc$(Routinename$)&".br") Then
75118 !        Execute "LOAD "&Lwrc$(Routinename$)&", RESIDENT"
75120 !     End If
75122     let fnestablishlibrarylinkage
75124 ! 
75126     let routinename$=uprc$(trim$(routinename$))
75128     if fnreadscreen(routinename$, mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1) then !  Successful Read
75130       let fninitializescreen
75132       if displayonly then 
75134         if trytocontinue then ! if we forced the continue then
75136 ! .            ! We must be in ReadOnly mode because we forced the continue
75138 ! .            !  rather then because of the DisplayOnly parameter,
75140 ! .            !  so screenio locking is off, and we should just pause
75142           input #0, fields str$(row+1)&","&str$(col+1)&",C 1,AEX" : kp$
75144         else 
75146 ! .            ! We're here because of DisplayOnly parameter. Pass back
75148 ! .            !  the window number and then suppress it to keep the window
75150 ! .            !  from being closed. The Calling Program will close it later.
75152           let returnwindownumber=wwindow
75154           let wwindow=0
75156         end if 
75158       else 
75160         let fnrunscreen
75162       end if 
75164       let fnclosescreen
75166       if returnwindownumber then 
75168         let key$=str$(returnwindownumber)
75170       end if 
75172     else 
75174       let msgbox("The screen "&trim$(routinename$)&" could not be found.")
75176     end if 
75178     let fnclosescreenfiles
75180     let fnscreenio$=key$
75182   fnend 
75184 ! 
75186   dim quitonly,saveandquit,selectandquit,quitother,asksaveandquit,reload,autoreload
75188   def fndefineexitmodes ! Configure Exit Value Constants
75190     let quitonly=1
75192     let saveandquit=2
75194     let selectandquit=3
75196     let quitother=4
75198     let asksaveandquit=5
75200     let reload=6
75202     let autoreload=7
75204   fnend 
75206 ! 
75208 ! .! =================================================================
75210 INITIALIZESCREEN: ! This Function Contains The Portion Of The Runtime Engine For Loading And Displaying Your Screen
75212 ! .! =================================================================
75214   def fninitializescreen(;___,r_,c_,warnwindow,populaterowone)
75216 ! 
75218     mat selectedkeys$(0)
75220     mat selectedrecords(0)
75222 ! 
75224     if screenio(si_border) or pos(uprc$(screenio$(si_attributes)),"BORDER") then 
75226       let _bordersize=2
75228     end if 
75230 ! 
75232     if env$("guimode")<>"" then 
75234       if fnrequiresgui(mat screenio$,mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
75236         if env$("guimode")="OFF" then 
75238           let fnpreservenongui
75240           let turnguibackoff=1
75242         end if 
75244 ! 
75246         let fnreadscreensize(rows,cols,parentwindow)
75248 ! 
75250         if row and col then 
75252           let r_=row-1-(_bordersize/2)
75254           let c_=col-1-(_bordersize/2)
75256         end if 
75258         if pos(lwrc$(screenio$(si_attributes)),"parent=none")<1 then 
75260           if (r_+screenio(si_vsize)+_bordersize)>rows or (c_+screenio(si_hsize)+_bordersize)>cols then 
75262             if parentwindow then 
75264               if (2==msgbox("This window ("&trim$(screenio$(si_screencode))&") is bigger then its parent window (#"&str$(parentwindow)&"). Do you wish to debug?","Error","yN")) then 
75266                 pause 
75268               end if 
75270               let exitmode=1
75272             else 
75274               let oldrows=rows
75276               let oldcols=cols
75278               let rows=max(r_+screenio(si_vsize)+_bordersize,rows)
75280               let cols=max(c_+screenio(si_hsize)+_bordersize,cols)
75282               open #0: "rows="&str$(rows)&", cols="&str$(cols),display,outin 
75284             end if 
75286           end if 
75288         end if 
75290         if trim$(screenio$(si_caption))<>"" then 
75292           if len(trim$(env$("Window_Caption"))) then 
75294             let oldwindowcaption$=trim$(env$("Window_Caption"))
75296           end if 
75298           print #0, border: trim$(screenio$(si_caption))
75300           let setenv("Window_Caption",trim$(screenio$(si_caption)))
75302         end if 
75304 !            Let Fnclearwindowsmenu
75306       end if 
75308     else 
75310       if fnrequiresgui(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
75312         print "This screen requires a new gui version of BR"
75314         let exitmode=1
75316       end if 
75318     end if 
75320 ! 
75322     if ~row or ~col then 
75324       if rows=0 or cols=0 then 
75326         let fnreadscreensize(rows,cols,parentwindow)
75328       end if 
75330       let row=int((rows-(screenio(si_vsize)+_bordersize))/2)+1+int(_bordersize/2)
75332       let col=int((cols-(screenio(si_hsize)+_bordersize))/2)+1+int(_bordersize/2)
75334     end if 
75336 ! 
75338     if ~exitmode then 
75340       let fnsetforcevisibility(0)
75342 ! 
75344       let wwindow=fnopenwindow(row,col,mat screenio$,mat screenio,parentwindow,displayonly,forcethisindex,active,editing)
75346 ! 
75348       let fnresetforcevisibility
75350       let fninitializemats(mat s$,mat screensubs$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
75352       let fnexecutesetsubscripts(mat screensubs$,"sio_")
75354       let fnexecutesetsubscripts(mat controlname$,"ctl_")
75356       let fnexecuteuniquesubscripts(mat controlname$,"ctl_")
75358 ! 
75360       mat fieldsssubs$(0)
75362       mat fieldsnsubs$(0)
75364 ! 
75366       if ~editing or pos(uprc$(env$("filter")),"Y") then 
75368         if ~editing and setting_enablelogging then 
75370           let fnlog("ScreenIO: Entering "&screenio$(si_screencode)&" screen.",setting_screenfolder$&"\"&screenio$(si_screencode))
75372         end if 
75374 ! 
75376         if trim$(screenio$(si_filelay))<>"" and fndoeslayoutexist(trim$(screenio$(si_filelay))) then 
75378           if usemyf then 
75380             mat f$(udim(mat myf$))
75382             mat f(udim(mat myf))
75384             if ~sum(myf) then 
75386               for index=1 to udim(mat myf$)
75388                 if len(trim$(myf$(index))) then 
75390                   let myfread=1
75392                 end if 
75394               next index
75396             else 
75398               let myfread=1
75400             end if 
75402           else 
75404             let fdatafile=fnopen(trim$(screenio$(si_filelay)),mat f$,mat f,mat form$,0,screenio(si_readindex),0,path$)
75406           end if 
75408 ! 
75410           let fnreadlayoutarrays(trim$(screenio$(si_filelay)),prefix$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldssspec$,mat fieldsnspec$,mat fieldssdescription$,mat fieldsndescription$)
75412           for index=1 to udim(mat fieldsssubs$)
75414             let fieldsssubs$(index)=lwrc$(trim$(fieldsssubs$(index)))
75416           next index
75418           for index=1 to udim(mat fieldsnsubs$)
75420             let fieldsnsubs$(index)=lwrc$(trim$(fieldsnsubs$(index)))
75422           next index
75424 ! 
75426 !    .         ! Here is where we link to our DEFAULT ENTER event
75428           if exists(setting_functionfolder$&"defaults\") then 
75430             if exists(setting_functionfolder$&"defaults\enter.brs") then 
75432               let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\enter.brs"))
75434             end if 
75436           end if 
75438 ! 
75440 !    .         ! Here is where we link to our ENTER event
75442           if len(trim$(screenio$(si_enterfn))) then 
75444             let fnexecute(screenio$(si_enterfn))
75446           end if 
75448 ! 
75450           let trytocontinue=0
75452 ! 
75454           if key$<>"" then 
75456             if displayonly then 
75458               read #fdatafile, using form$(fdatafile), key=fnkey$(fdatafile,key$), release: mat f$, mat f nokey ignore
75460             else if screenio(si_screeniolocking) then 
75462 ! .                  ! We have to read the record first to ensure the record isn't locked by any legacy applications.
75464               let warnwindow=fndisplayloadmessage
75466               read #fdatafile, using form$(fdatafile), key=fnkey$(fdatafile,key$) : mat f$, mat f nokey ignore locked ERRORFILEISLOCKED
75468               close #warnwindow: 
75470               if trytocontinue then 
75472                 read #fdatafile, using form$(fdatafile), key=fnkey$(fdatafile,key$), release: mat f$, mat f nokey ignore
75474               end if 
75476               if ~exitmode then 
75478                 reread #fdatafile, using form$(fdatafile), release: mat f$, mat f nokey ignore
75480               end if 
75482             else 
75484               let warnwindow=fndisplayloadmessage
75486               read #fdatafile, using form$(fdatafile), key=fnkey$(fdatafile,key$) : mat f$, mat f nokey ignore locked ERRORFILEISLOCKED
75488               close #warnwindow: 
75490               if trytocontinue then 
75492                 read #fdatafile, using form$(fdatafile), key=fnkey$(fdatafile,key$), release: mat f$, mat f nokey ignore
75494                 let displayonly=1
75496               end if 
75498             end if 
75500             mat read_f$(udim(mat f$))=f$
75502             mat read_f(udim(mat f))=f
75504 ! 
75506             if file(fdatafile)<>0 then ! Key Not Found
75508               if exitmode=0 then 
75510 ! 
75512 ! .                     ! .   ! Here's where we link to our NoKey event
75514                 if len(trim$(screenio$(si_nokeyfn))) then 
75516                   let fnexecute(screenio$(si_nokeyfn))
75518                 else 
75520 ! 
75522                   if exists(setting_functionfolder$&"defaults\") then 
75524                     if exists(setting_functionfolder$&"defaults\nokey.brs") then 
75526                       let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\nokey.brs"))
75528                     else 
75530                       let msgbox("The '"&key$&"' record could not be found.","Not Found","Ok","Err")
75532                       let exitmode=1
75534                     end if 
75536                   else 
75538                     let msgbox("The '"&key$&"' record could not be found.","Not Found","Ok","Err")
75540                     let exitmode=1
75542                   end if 
75544                 end if 
75546               end if 
75548             else 
75550               let currentkey$=fnbuildkey$(trim$(screenio$(si_filelay)),mat f$,mat f,screenio(si_returnindex))
75552               let currentrec=rec(fdatafile)
75554 ! 
75556               if exists(setting_functionfolder$&"defaults\") then 
75558                 if exists(setting_functionfolder$&"defaults\read.brs") then 
75560                   let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\read.brs"))
75562                 end if 
75564               end if 
75566 ! 
75568 !    .               ! Here is where we link to our READ event
75570               if ~editing and setting_enablelogging then let fnlogarray(mat f$,mat f,"ScreenIO: Reading "&screenio$(si_filelay)&" file, Record: "&str$(currentrec),setting_screenfolder$&"\"&screenio$(si_screencode))
75572               if len(trim$(screenio$(si_readfn))) then 
75574                 let fnexecute(screenio$(si_readfn))
75576               end if 
75578             end if 
75580           else if record>0 then 
75582             if displayonly then 
75584               read #fdatafile, using form$(fdatafile), rec=record, release: mat f$, mat f norec ignore
75586             else if screenio(si_screeniolocking) then 
75588 ! .                  ! We have to read the record first to ensure the record isn't locked by any legacy applications.
75590               let warnwindow=fndisplayloadmessage
75592               read #fdatafile, using form$(fdatafile), rec=record : mat f$, mat f norec ignore locked ERRORFILEISLOCKED
75594               close #warnwindow: 
75596               if trytocontinue then 
75598                 read #fdatafile, using form$(fdatafile), rec=record, release: mat f$, mat f norec ignore
75600               end if 
75602               if ~exitmode then 
75604                 reread #fdatafile, using form$(fdatafile), release: mat f$, mat f norec ignore
75606               end if 
75608             else 
75610               let warnwindow=fndisplayloadmessage
75612               read #fdatafile, using form$(fdatafile), rec=record : mat f$, mat f norec ignore locked ERRORFILEISLOCKED
75614               close #warnwindow: 
75616               if trytocontinue then 
75618                 read #fdatafile, using form$(fdatafile), rec=record, release: mat f$, mat f nokey ignore
75620                 let displayonly=1
75622               end if 
75624             end if 
75626 ! 
75628             mat read_f$(udim(mat f$))=f$
75630             mat read_f(udim(mat f))=f
75632 ! 
75634             if file(fdatafile)<>0 then ! Key Not Found
75636               if exitmode=0 then 
75638                 if len(trim$(screenio$(si_nokeyfn))) then 
75640                   let fnexecute(screenio$(si_nokeyfn))
75642                 else 
75644                   if exists(setting_functionfolder$&"defaults\") then 
75646                     if exists(setting_functionfolder$&"defaults\nokey.brs") then 
75648                       let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\nokey.brs"))
75650                     else 
75652                       let msgbox("The '"&key$&"' record could not be found.","Not Found","Ok","Err")
75654                       let exitmode=1
75656                     end if 
75658                   else 
75660                     let msgbox("The '"&key$&"' record could not be found.","Not Found","Ok","Err")
75662                     let exitmode=1
75664                   end if 
75666                 end if 
75668               end if 
75670             else 
75672 !    .               ! Here is where we link to our READ event
75674               let currentkey$=fnbuildkey$(trim$(screenio$(si_filelay)),mat f$,mat f,screenio(si_returnindex))
75676               let currentrec=rec(fdatafile)
75678               if ~editing and setting_enablelogging then let fnlogarray(mat f$,mat f,"ScreenIO: Reading "&screenio$(si_filelay)&" file, Record: "&str$(currentrec)&" Key: "&currentkey$,setting_screenfolder$&"\"&screenio$(si_screencode))
75680 ! 
75682               if exists(setting_functionfolder$&"defaults\") then 
75684                 if exists(setting_functionfolder$&"defaults\read.brs") then 
75686                   let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\read.brs"))
75688                 end if 
75690               end if 
75692 ! 
75694               if len(trim$(screenio$(si_readfn))) then 
75696                 let fnexecute(screenio$(si_readfn))
75698               end if 
75700             end if 
75702           else if usemyf and myfread then 
75704             mat f$=myf$
75706             mat f=myf
75708 ! 
75710             mat read_f$(udim(mat f$))=f$
75712             mat read_f(udim(mat f))=f
75714 ! 
75716             if exists(setting_functionfolder$&"defaults\") then 
75718               if exists(setting_functionfolder$&"defaults\read.brs") then 
75720                 let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\read.brs"))
75722               end if 
75724             end if 
75726 ! 
75728 !    .            ! Here is where we link to our READ event
75730             if ~editing and setting_enablelogging then let fnlogarray(mat f$,mat f,"ScreenIO: Data passed in",setting_screenfolder$&"\"&screenio$(si_screencode))
75732             if len(trim$(screenio$(si_readfn))) then 
75734               let fnexecute(screenio$(si_readfn))
75736             end if 
75738           else 
75740             mat read_f$(udim(mat f$))=f$
75742             mat read_f(udim(mat f))=f
75744 ! 
75746 !    .            ! Here is where we link to our INITIALIZE event
75748             if exists(setting_functionfolder$&"defaults") then 
75750               if exists(setting_functionfolder$&"defaults\init.brs") then 
75752                 let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\init.brs"))
75754               end if 
75756             end if 
75758 ! 
75760             if len(trim$(screenio$(si_initfn))) then 
75762               let fnexecute(screenio$(si_initfn))
75764             end if 
75766             if ~editing and setting_enablelogging then let fnlogarray(mat f$,mat f,"ScreenIO: Initializing Record for "&screenio$(si_filelay)&" file.",setting_screenfolder$&"\"&screenio$(si_screencode))
75768           end if 
75770         else 
75772           mat read_f$(udim(mat f$))=f$
75774           mat read_f(udim(mat f))=f
75776 ! 
75778           if exists(setting_functionfolder$&"defaults\") then 
75780             if exists(setting_functionfolder$&"defaults\enter.brs") then 
75782               let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\enter.brs"))
75784             end if 
75786           end if 
75788 ! 
75790 !    .         ! Here is where we link to our ENTER event
75792           if len(trim$(screenio$(si_enterfn))) then 
75794             let fnexecute(screenio$(si_enterfn))
75796           end if 
75798         end if 
75800       end if 
75802     end if 
75804 ! 
75806 ! .   ! Draw the screen and populate any listviews
75808     if ~exitmode then 
75810       if displayonly then let forceindex=wwindow+1000
75812       if forcethisindex then let forceindex=forcethisindex
75814 ! 
75816       let fnpopulatecombo(wwindow,fdatafile,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat form$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,(editing and ~pos(uprc$(env$("filter")),"Y")),editing)
75818 ! 
75820 ! .      Predisplay Output Specs
75822       let fnchangeforcevisibility(0)
75824       let fndrawframes(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1,forceindex)
75826       if ~editing then let fndrawscreens(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1,forceindex)
75828       let fngenerateoutputspecs(wwindow,mat s_data$,mat s_spec$,mat s_help$,mat s$,mat screensubs$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$,forceindex)
75830       let fnchangeforcevisibility(1)
75832       if udim(mat s_spec$) then 
75834         print #wwindow, fields mat s_spec$, help mat s_help$ : mat s_data$
75836       end if 
75838       mat temp_s_spec$(udim(mat s_spec$))=s_spec$
75840 ! 
75842 ! .      Predisplay Input Specs
75844       let fngenerateinputspecs(wwindow,mat s_data$,mat s_data,mat s_spec$,mat s_subs,mat s_help$,mat s_old$,mat f$,mat f,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$,forceindex)
75846       if ~(udim(mat s_data)) then 
75848         print #wwindow, fields mat s_spec$, help mat s_help$ : mat s_data$
75850       end if 
75852 ! 
75854       if fntheresalistview(mat fieldtype$) then 
75856         let fndrawalllistviews(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1,forceindex)
75858         let lastrow=fnpopulatealllistviews(wwindow,fdatafile,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat form$,mat listviewrecords,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,(editing and ~pos(uprc$(env$("filter")),"Y")),editing,populaterowone,displayonly)
75860         mat old_data=(0) ! Clear information so it forces read again.
75862         mat rlv_savereaddata$=("")
75864         if fn43 then let currentrow=populaterowone ! In 4.3, sorted listviews don't start at the top, so we have to force them.
75866         let s_currentrow=0
75868         if listviewindex and currentrow<>s_currentrow then 
75870           let curfld(udim(mat s_data$)+1,currentrow)
75872         end if 
75874         mat old_data(udim(mat s_data))
75876 ! 
75878 ! .         ! Preread the listview in case they have a read event, so it runs the currently selected item first
75880         if listviewindex and udim(mat s_data) then 
75882           let s_data(1)=1
75884           let fnreadlistviews(function,mat f$, mat f, currentkey$,currentrec,mat selectedkeys$,mat selectedrecords,mat s_data,mat s_spec$,listviewindex,mat old_data,wwindow,fdatafile,mat form$, mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,nextrow)
75886         end if 
75888       end if 
75890 ! 
75892       if ~editing or pos(uprc$(env$("filter")),"Y") or len(trim$(screenio$(si_loadfn))) then 
75894         if displayonly then 
75896 ! .            redisplay Output Specs
75898           if repopulatecombo then let fnpopulatecombo(wwindow,fdatafile,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat form$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,0,0,repopulatecombo) : let repopulatecombo=0
75900           if redrawframes then let fndrawframes(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1,forceindex, redrawframes) : let redrawframes=0
75902           if redrawscreens then let fndrawscreens(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1,forceindex, redrawscreens) : let redrawscreens=0
75904           let fngenerateoutputspecs(wwindow,mat s_data$,mat s_spec$,mat s_help$,mat s$,mat screensubs$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$,forceindex)
75906           if udim(mat s_spec$) then 
75908             print #wwindow, fields mat s_spec$, help mat s_help$ : mat s_data$
75910           end if 
75912           mat temp_s_spec$(udim(mat s_spec$))=s_spec$
75914 ! 
75916 ! .!    .      redisplay Input Specs
75918           let fngenerateinputspecs(wwindow,mat s_data$,mat s_data,mat s_spec$,mat s_subs,mat s_help$,mat s_old$,mat f$,mat f,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$,forceindex)
75920           if ~(udim(mat s_data)) then 
75922             print #wwindow, fields mat s_spec$, help mat s_help$ : mat s_data$
75924           end if 
75926         end if 
75928       end if 
75930 ! 
75932 !       ! Here is where we link to our LOAD event
75934       if len(trim$(screenio$(si_loadfn))) then 
75936         let fnexecute(screenio$(si_loadfn))
75938       end if 
75940     end if 
75942 ! 
75944   fnend 
75946 ! 
75948 ! .! =================================================================
75950 RUNSCREEN: ! This Function Contains The Portion Of The Runtime Engine For Actually Using Your Screen.
75952 ! .! =================================================================
75954   def fnrunscreen(;___,waittime,ch,theresanl,savescreendata,loopagain,warnwindow,findstickies,position,listviewspec$*40,maxrow)
75956 ! 
75958     let fkey(-1)
75960 ! 
75962     mat save_f(udim(mat f))=f
75964     mat save_f$(udim(mat f$))=f$
75966     mat save_s$(udim(mat s$))=s$
75968 ! 
75970     do until exitmode
75972 ! .! .   If There's Output Specs, Generate Output Specs And Print Output Controls (Buttons, Pictures, Captions)
75974       if repopulatecombo then 
75976         let scr_freeze
75978         let fnpopulatecombo(wwindow,fdatafile,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat form$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,0,0,repopulatecombo)
75980         let repopulatecombo=0
75982       end if 
75984       if redrawframes then 
75986         let scr_freeze
75988         let fndrawframes(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1,forceindex,redrawframes)
75990         let redrawframes=0
75992       end if 
75994       if redrawscreens then 
75996         let scr_freeze
75998         let fndrawscreens(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1,forceindex,redrawscreens)
76000         let redrawscreens=0
76002       end if 
76004 ! 
76006       let fneraseinvisible(wwindow,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
76008 ! 
76010       let fngenerateoutputspecs(wwindow,mat s_data$,mat s_spec$,mat s_help$,mat s$,mat screensubs$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$)
76012       if udim(mat s_spec$) then 
76014         let fnchangeforcevisibility(1)
76016         let fnblankoldsfields$(mat s_spec$,mat temp_s_spec$,wwindow)
76018         print #wwindow, fields mat s_spec$, help mat s_help$ : mat s_data$
76020         let fnresetforcevisibility
76022       end if 
76024       mat temp_s_spec$(udim(mat s_spec$))=s_spec$
76026 ! 
76028 ! .      ! we need to do the same thing for input specs that we do for output specs, in case invisible changed.
76030 ! .      !  but we need to make sure this only happens if it has to.
76032 ! .      ! dim Temp_SI_Spec$(1)*255
76034       let fngenerateinputspecs(wwindow,mat s_data$,mat s_data,mat s_spec$,mat s_subs,mat s_help$,mat s_old$,mat f$,mat f,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$)
76036 ! .      ! if udim(mat Temp_SI_Spec$) then
76038 ! .      !    let Fnblankoldsfields$(Mat S_Spec$,Mat Temp_SI_Spec$,Wwindow)
76040 ! .      ! end if
76042 ! .      ! mat Temp_SI_Spec$(Udim(Mat S_Spec$))=S_Spec$
76044 ! 
76046 ! 
76048       if listviewindex then let theresanl=fnattribute("L",listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
76050 ! 
76052       if screenio(si_waittime) then 
76054         let waittime=screenio(si_waittime)
76056       else 
76058         let waittime=-1
76060       end if 
76062 ! 
76064       if udim(mat s_data$) + udim(mat s_data) then 
76066 ! 
76068 ! 
76070 ! .         ! Handle repositioning of Current Row by Record or by Row if either is given.
76072         if listviewindex then 
76074 ! 
76076 ! .            ! If CurrentRow is negative, treat like CurrentRec is negative.
76078           if currentrow<>s_currentrow and currentrec=s_currentrec and currentrow<0 then 
76080             let currentrec=currentrow
76082             let currentrow=s_currentrow
76084           end if 
76086 ! 
76088           if currentrec<>s_currentrec then 
76090             if currentrec<0 then 
76092               let listviewspec$=s_spec$(udim(mat s_spec$))
76094               let listviewspec$=listviewspec$(1:pos(listviewspec$,",",-1)-1)
76096               let listviewspec$=listviewspec$(1:pos(listviewspec$,",",-1)-1)
76098 ! 
76100               if fn43 then 
76102                 input #wwindow, fields listviewspec$&",ROWSUB,ALL,DISPLAYED_ORDER,NOWAIT" : mat populatesortedsubs
76104                 let maxrow=udim(mat populatesortedsubs)
76106 ! 
76108                 if currentrec=-1 then 
76110                   let currentrow=populatesortedsubs(1)
76112                 else if currentrec<=-2 then 
76114                   let currentrow=populatesortedsubs(maxrow)
76116                 else if currentrec>-2 and currentrec<-1 then 
76118                   let currentrow=populatesortedsubs(max(min(1,int(maxrow*(currentrec+1)*(-1))),maxrow))
76120                 else if currentrec>-1 and currentrec<0 then 
76122                   let currentrow=populatesortedsubs(max(min(1,int(maxrow*currentrec*(-1))),maxrow))
76124                 end if 
76126               else 
76128                 input #wwindow, fields listviewspec$&",ROWCNT,ALL" : maxrow
76130 ! 
76132                 if currentrec=-1 then 
76134                   let currentrow=1
76136                 else if currentrec<=-2 then 
76138                   let currentrow=maxrow
76140                 else if currentrec>-2 and currentrec<-1 then 
76142                   let currentrow=max(min(1,int(maxrow*(currentrec+1)*(-1))),maxrow)
76144                 else if currentrec>-1 and currentrec<0 then 
76146                   let currentrow=max(min(1,int(maxrow*currentrec*(-1))),maxrow)
76148                 end if 
76150               end if 
76152             else if currentrec>0 then 
76154               if srch(mat lv_records,currentrec)>0 then 
76156                 let currentrow=srch(mat listviewrecords,currentrec)
76158               end if 
76160             end if 
76162           end if 
76164           if currentrow<>s_currentrow then 
76166             let curfld(udim(mat s_data$)+1,currentrow)
76168           end if 
76170         end if 
76172 ! 
76174         mat old_data(udim(mat s_data))
76176 ! .         ! mat Old_Data(Udim(Mat S_Data))=S_Data
76178 ! .         ! let msgbox("Setting Old_Data(1)="&str$(Old_Data(1))&" : S_Data(1)="&str$(S_Data(1)))
76180 ! 
76182 ! .         ! Apply CurrentField$ if its been changed.
76184         if currentfield$<>s_currentfield$ then 
76186           if currentfield$<>"" then 
76188             let tempcontrol=0
76190             let tempfield=0
76192             let foundalready=0
76194             let function=0
76196 ! 
76198             for tempindex=1 to udim(mat controlname$)
76200               if lwrc$(trim$(currentfield$))=lwrc$(fnuniquename$(mat controlname$,tempindex)) then 
76202                 let tempcontrol=tempindex
76204                 let foundalready=1
76206               else if ((~foundalready) and (lwrc$(trim$(currentfield$))=lwrc$(trim$(controlname$(tempindex))))) then 
76208                 let tempcontrol=tempindex
76210               end if 
76212             next tempindex
76214             if tempcontrol then 
76216               let tempfield=srch(mat s_subs,tempcontrol)
76218             end if 
76220             if tempfield>0 then let currentfield=tempfield
76222           end if 
76224         end if 
76226 ! 
76228         if listviewindex and currentfield=udim(mat s_spec$) then 
76230           if function=106 or function=105 then 
76232             let curfld(currentfield,currentrow)
76234           end if 
76236         else ! If Current Field Is Not The Listview Then Update Current Field Too.
76238           if currentfield then let curfld(currentfield,function)
76240         end if 
76242 ! 
76244 ! .         ! Draw the Record Locking information on the screen
76246         if udim(mat other_s_data$) then 
76248 ! .            ! Check to see if the other user changed anything
76250           let fnfindchanges(mat other_s_data$,mat other_s_subs,mat s_data$,mat s_subs,mat otherchanged_s_data$,mat otherchanged_s_subs)
76252           mat other_s_data$(0)
76254           mat other_s_subs(0)
76256         end if 
76258 ! 
76260         if udim(mat otherchanged_s_subs) then 
76262 ! .            ! If they did, then color their changes pink. (Make color defineable)
76264           let fngenerateinputspecs(wwindow,mat s_data$,mat s_data,mat s_spec$,mat s_subs,mat s_help$,mat s_old$,mat f$,mat f,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$,0,mat otherchanged_s_subs)
76266         end if 
76268 ! 
76270 ! .         ! Draw the Yellow stickynotes that contain the users changes.
76272         mat changewindownumbers(udim(mat changed_s_subs))
76274         for index=1 to udim(mat changed_s_subs)
76276           if changed_s_subs(index)>0 then 
76278             if changewindownumbers(index)=0 then 
76280               let fnopensticky(index,changed_s_data$(index),changewindownumbers(index),changed_s_subs(index),rows,cols,row,col,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
76282             end if 
76284           end if 
76286         next index
76288 ! 
76290 ! .         ! Save Screen Data for later
76292         if ~savescreendata then 
76294           mat save_s_data$(udim(mat s_data$))=s_data$
76296           mat save_s_subs(udim(mat s_subs))=s_subs
76298           let savescreendata=1
76300         end if 
76302 ! 
76304 ! .         ! Print Screen Data
76306         let fnchangeforcevisibility(1)
76308         do 
76310           let loopagain=0
76312           if ~(udim(mat s_data$)) then 
76314             if len(trim$(screenio$(si_inputattr))) then 
76316               rinput #wwindow, fields mat s_spec$, attr screenio$(si_inputattr), wait=waittime : mat s_data timeout TIMEOUTERROR
76318             else 
76320               rinput #wwindow, fields mat s_spec$, wait=waittime : mat s_data timeout TIMEOUTERROR
76322             end if 
76324           else 
76326             if len(trim$(screenio$(si_inputattr))) then 
76328               rinput #wwindow, fields mat s_spec$, attr screenio$(si_inputattr), wait=waittime, help mat s_help$ : mat s_data$, mat s_data timeout TIMEOUTERROR
76330             else 
76332               rinput #wwindow, fields mat s_spec$, wait=waittime, help mat s_help$ : mat s_data$, mat s_data timeout TIMEOUTERROR
76334             end if 
76336           end if 
76338           if udim(s_data)<1 then 
76340             mat s_data(1)
76342             if listviewindex and multiselect(listviewindex) then 
76344               let s_data(1)=0
76346             else 
76348               let s_data(1)=currentrow
76350             end if 
76352           end if 
76354 ! .            ! print nxtfld; curfld : fkey linput X$
76356           let function=fkey
76358           let currentfield=curfld
76360           let nextfield=nxtfld
76362 ! 
76364 ! .            ! Make L Attribute Work In Listviews - Special Thanks To Susan Smith (Possibly deprecated, looks like BR just works this way by default now.)
76366 ! .            ! If there's an L, and they're in the listview, at the top or bottom, and we're here cause they pressed Up or Dn, then go back in the loop, don't let it loop around.
76368           if (theresanl and udim(mat s_data) and currentfield=udim(mat s_data$)+1 and (((function=105 or function=90) and currow=1) or ((function=106 or function=91) and currow=lastrow))) then 
76370             let loopagain=1
76372           end if 
76374 ! 
76376 ! .            ! If they press escape, erase all the yellow stickies and make them go away.
76378           if function=99 and udim(mat changewindownumbers) then 
76380             let fncloseallstickies(mat changewindownumbers,mat changed_s_subs,mat changed_s_data$)
76382             let loopagain=1
76384           end if 
76386 ! 
76388 ! .            ! If they click on one of the stickies, then put their changes
76390 ! .            !  into mat s_data and go back up again.
76392           if function>fnstickybase and function<=fnstickybase+udim(mat changed_s_subs) then 
76394             let index=srch(mat s_subs,changed_s_subs(function-fnstickybase))
76396             if index>0 and index<udim(mat s_data$) then 
76398               let s_data$(index)=changed_s_data$(function-fnstickybase)
76400               close #changewindownumbers(function-fnstickybase): 
76402               let fnconsolidatedirect(mat changewindownumbers,function-fnstickybase)
76404               let fnconsolidatedirect(mat changed_s_subs,function-fnstickybase)
76406               let fnconsolidatedirects(mat changed_s_data$,function-fnstickybase)
76408             end if 
76410             let loopagain=1
76412           end if 
76414 ! 
76416           if ~fn42ia then ! after this version we handle multiline textboxes another way
76418             if currentfield<=udim(mat s_subs) and (function=0 or function=fnkeybase+s_subs(currentfield)) and fnmultilinetextbox(s_subs(currentfield),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then ! enter in a multi-line textbox
76420               let position=(((currow-1)*width(s_subs(currentfield)))+curcol) ! Find The Position
76422               if len(trim$(s_data$(currentfield)))<=specwidth(s_subs(currentfield))-3 then ! if there's room for an Enter
76424                 let s_data$(currentfield)(position:position-1)=chr$(13)&chr$(10)
76426                 let position+=2 ! Move to after the crlf$
76428                 let s_data$(currentfield)=srep$(trim$(s_data$(currentfield)),chr$(1),"")&chr$(1)
76430               else 
76432                 print bell
76434               end if 
76436               let position=min(position,specwidth(s_subs(currentfield)))
76438               let s_spec$(currentfield)=fnsetcurserposition$(s_spec$(currentfield),position)
76440               let loopagain=1
76442               let curfld(currentfield)
76444             end if 
76446           end if 
76448         loop while loopagain
76450 ! 
76452         if ~fn42ia then ! after this version we handle multiline textboxes another way
76454           for index=1 to udim(mat s_subs)
76456             if fnmultilinetextbox(s_subs(index),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
76458               let s_data$(index)=srep$(s_data$(index),chr$(1),"")
76460             end if 
76462           next index
76464         end if 
76466 ! 
76468         let fnresetforcevisibility
76470         if currentfield>0 and currentfield<=udim(mat s_subs) then 
76472           let currentfield$=fnuniquename$(mat controlname$,s_subs(currentfield))
76474           let s_currentfield$=currentfield$
76476         else 
76478           let currentfield$=fnuniquename$(mat controlname$,listviewindex)
76480           let s_currentfield$=currentfield$
76482         end if 
76484 ! 
76486 ! .         ! Read the listviews if there are any
76488 ! .         ! This function breaks curfld by inputting from the grid. this means we have to monkey around a lot more above.
76490 ! .         !  At this point, s_data(1) represents the count.
76492 ! .         ! let TempCurfld=Curfld ! this logic didn't work, because 2d controls
76494         let fnreadlistviews(function,mat f$, mat f, currentkey$,currentrec,mat selectedkeys$,mat selectedrecords,mat s_data,mat s_spec$,listviewindex,mat old_data,wwindow,fdatafile,mat form$, mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,nextrow)
76496 ! .         ! let Curfld(TempCurfld)
76498 ! .         ! At this point, s_data(1) represents the current row.
76500         if listviewindex then 
76502           let currentrow=s_currentrow=s_data(1)
76504           let s_currentrec=currentrec
76506         end if 
76508 ! 
76510         if function=99 and fnsearchclosely(mat function$,"exitmode","quitonly") then 
76512 ! .            ! They pressed esc but the program already has exit buttons, so simply ignore latest changes
76514         else 
76516           let fnvalidateandsave(mat s_data$,mat s_spec$,mat s_subs,mat s_old$,mat f$,mat f,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
76518         end if 
76520 ! 
76522       else ! Nothing Is Selected
76524 ! .         ! so preform a hidden input
76526         let fnchangeforcevisibility(0)
76528         input #0, fields str$(row+1)&","&str$(col+1)&",C 1,AEX", wait=waittime : kp$ error TIMEOUTERROR
76530         let function=fkey
76532         let fnresetforcevisibility
76534       end if 
76536 ! 
76538       let fnrespondtouseraction(function,exitmode,currentfield,currentfield$,mat s_subs,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,wwindow,savedontask)
76540 ! 
76542       if exitmode=asksaveandquit then 
76544         if ~fnsamea(mat save_f,mat f) or ~fnsameas(mat save_f$,mat f$) or ~fnsameas(mat save_s$,mat s$) then 
76546           if len(trim$(screenio$(si_filelay))) then 
76548             if usemyf then 
76550               let ch=msgbox("The data has changed. Do you want to accept the changes?","Save?","ynC","QST")
76552             else 
76554               let ch=msgbox("The record has changed. Would you like to save the record?","Save?","ynC","QST")
76556             end if 
76558           else ! No file layout, can't save
76560             let ch=3
76562           end if 
76564           let exitmode=0
76566           if ch=2 then let exitmode=saveandquit
76568           if ch=3 then let exitmode=quitonly
76570         else 
76572           let exitmode=quitonly
76574         end if 
76576       end if 
76578       if exitmode=saveandquit then 
76580 ! .         ! Execute our preWRITE Event Here
76582         if exists(setting_functionfolder$&"defaults\") then 
76584           if exists(setting_functionfolder$&"defaults\write.brs") then 
76586             let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\write.brs"))
76588           end if 
76590         end if 
76592 ! 
76594         if len(trim$(screenio$(si_writefn))) then 
76596           let fnexecute(screenio$(si_writefn))
76598           if exitmode=0 and parentwindow and fkey=92 then ! Probably Tabs and the exit was cancelled
76600 ! .               ! Set the current tab back
76602             input #parentwindow, fields str$(row+1)&","&str$(col+1)&",C 1,,NOWAIT" : dummy$
76604             print #parentwindow, fields str$(row+1)&","&str$(col+1)&",C 1" : ""
76606           end if 
76608         end if 
76610       end if 
76612 ! 
76614       if exitmode=saveandquit then 
76616         if usemyf then 
76618           mat myf$=f$
76620           mat myf=f
76622         else if udim(s_data$)>0 and fdatafile and file(fdatafile)>=0 then 
76624           mat disk_f$(udim(read_f$))=("")
76626           mat disk_f(udim(read_f))=(0)
76628           let warnwindow=fndisplayloadmessage
76630           if key$<>"" then 
76632             read #fdatafile, using form$(fdatafile), key=fnkey$(fdatafile,key$) : mat disk_f$, mat disk_f locked ERRORFILELOCKEDONEXIT
76634             let fnsetlogchanges(mat read_f$,mat disk_f)
76636           else if record<>0 then 
76638             read #fdatafile, using form$(fdatafile), rec=record : mat disk_f$,mat disk_f locked ERRORFILELOCKEDONEXIT
76640             let fnsetlogchanges(mat read_f$,mat disk_f)
76642           end if 
76644           close #warnwindow: 
76646           if ~fnsameas(mat disk_f$,mat read_f$) or ~fnsamea(mat disk_f,mat read_f) then 
76648             if setting_enablelogging then 
76650               let fnlogchanges(mat disk_f$,mat disk_f,"ScreenIO: "&screenio$(si_screencode)&" screen attempting to Merge changes in "&screenio$(si_filelay)&" file, Record: "&str$(rec(fdatafile))&" Key: "&key$&" Other User",program$&"\Screen: "&screenio$(si_screencode),screenio$(si_filelay))
76652               let fnlogchanges(mat f$,mat f,"ScreenIO: "&screenio$(si_screencode)&" screen attempting to Merge changes in "&screenio$(si_filelay)&" file, Record: "&str$(rec(fdatafile))&" Key: "&key$&" My Changes",program$&"\Screen: "&screenio$(si_screencode),screenio$(si_filelay))
76654             end if 
76656             if len(trim$(screenio$(si_mergefn))) then ! If a merge function is given
76658               let fnexecute(screenio$(si_mergefn)) ! use it
76660             else if screenio(si_screeniomerge) then ! If we selected "Auto Merge"
76662               for index=1 to udim(mat f$) ! take my changes and merge them into Other's Changes
76664                 if trim$(f$(index))<>trim$(read_f$(index)) then 
76666                   let disk_f$(index)=f$(index)
76668                 end if 
76670               next index
76672               for index=1 to udim(mat f)
76674                 if f(index)<>read_f(index) then 
76676                   let disk_f(index)=f(index)
76678                 end if 
76680               next index
76682               mat f$=disk_f$
76684               mat f=disk_f
76686             else if exists(setting_functionfolder$&"defaults\") and exists(setting_functionfolder$&"defaults\merge.brs") then 
76688               let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\merge.brs"))
76690             else ! Otherwise, allow the user to merge the records.
76692               let msgbox("The record was changed by another user. You have to make your changes again.","I'm Sorry")
76694               let currentkey$=key$
76696               let exitmode=reload
76698 ! 
76700 ! .                  ! Save old record to detect what other user changed
76702               mat other_s_data$(udim(mat save_s_data$))=save_s_data$
76704               mat other_s_subs(udim(mat save_s_subs))=save_s_subs
76706 ! 
76708 ! .                  ! Detect what I changed
76710               let findstickies=1
76712             end if 
76714           end if 
76716 ! 
76718           if exitmode=saveandquit then 
76720             if key$<>"" then 
76722               rewrite #fdatafile, using form$(fdatafile), key=fnkey$(fdatafile,key$) : mat f$, mat f
76724               if setting_enablelogging then let fnlogchanges(mat f$,mat f,"ScreenIO: Updating "&screenio$(si_filelay)&" file, Record: "&str$(rec(fdatafile))&" Key: "&key$,setting_screenfolder$&"\"&screenio$(si_screencode))
76726             else if record<>0 then 
76728               rewrite #fdatafile, using form$(fdatafile), rec=record : mat f$, mat f
76730               if setting_enablelogging then let fnlogchanges(mat f$,mat f,"ScreenIO: Updating "&screenio$(si_filelay)&" file, Record: "&str$(rec(fdatafile)),setting_screenfolder$&"\"&screenio$(si_screencode))
76732             else 
76734               write #fdatafile, using form$(fdatafile) : mat f$, mat f
76736               if setting_enablelogging then let fnlogarray(mat f$,mat f,"ScreenIO: Writing "&screenio$(si_filelay)&" file, Record: "&str$(rec(fdatafile)),setting_screenfolder$&"\"&screenio$(si_screencode))
76738             end if 
76740           end if 
76742         end if 
76744       end if 
76746     loop 
76748 ! 
76750 ! .   ! Set Return Key
76752     if exitmode>quitonly and exitmode<>quitother and exitmode<>reload and trim$(screenio$(si_filelay))<>"" then 
76754       let currentkey$=fnbuildkey$(trim$(screenio$(si_filelay)),mat f$,mat f,screenio(si_returnindex))
76756       if currentkey$="" then let currentkey$=str$(rec(fdatafile)) ! Pass Back Record If Key Doesn't exist
76758     end if 
76760 ! 
76762     let fncloseallstickies(mat changewindownumbers,mat changed_s_subs,mat changed_s_data$)
76764     if findstickies then let fnfindchanges(mat s_data$,mat s_subs,mat save_s_data$,mat save_s_subs,mat changed_s_data$,mat changed_s_subs)
76766 ! 
76768   fnend 
76770 ! 
76772 ! .! =================================================================
76774 CLOSESCREEN: ! This Function Contains The Portion Of The Runtime Engine That Closes Your Screen And Returns.
76776 ! .! =================================================================
76778   def fnclosescreen
76780     if ~editing and setting_enablelogging then 
76782       let fnlog("ScreenIO: Leaving Screen "&screenio$(si_screencode),setting_screenfolder$&"\"&screenio$(si_screencode))
76784     end if 
76786     if ~editing or pos(uprc$(env$("filter")),"Y") then 
76788 ! .   ! Execute our EXIT Event Here
76790       if exists(setting_functionfolder$&"defaults\") then 
76792         if exists(setting_functionfolder$&"defaults\exit.brs") then 
76794           let fnexecute(fnfunctionstring$(setting_functionfolder$&"defaults\exit.brs"))
76796         end if 
76798       end if 
76800 ! 
76802       if len(trim$(screenio$(si_exitfn))) then 
76804         let fnexecute(screenio$(si_exitfn))
76806       end if 
76808     end if 
76810 ! 
76812     if exitmode>quitonly then 
76814       let key$=currentkey$
76816     else 
76818       let key$="" ! Cancel Forces No Return Value
76820     end if 
76822 ! 
76824 ! .   ! Close Data Files
76826     if fdatafile and file(fdatafile)>=0 then 
76828       let fnclosefile(fdatafile,trim$(screenio$(si_filelay)),path$,1)
76830     end if 
76832 ! 
76834 ! .   ! Close Child Window
76836     if wwindow and file(wwindow)<>-1 then 
76838       close #wwindow: 
76840     end if 
76842 ! 
76844 !      If Librarylinkage Then
76846 !         Execute "clear "&Lwrc$(Routinename$)&" status"
76848 !      End If
76850 ! 
76852 ! .   ! Restore all screen settings to previous values
76854     if oldrows and oldcols then 
76856       open #0: "rows="&str$(oldrows)&",cols="&str$(oldcols), display, outin 
76858     end if 
76860     if len(oldwindowcaption$) then 
76862       print #0, border: oldwindowcaption$
76864       let setenv("Window_Caption",oldwindowcaption$)
76866     end if 
76868     let fnresetforcevisibility
76870     if turnguibackoff then 
76872       let fnrestorenongui
76874     end if 
76876   fnend 
76878 ! 
76880   def fnmultilinetextbox(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
76882     if height(index)>1 and fieldtype$(index)="C" then 
76884       let fnmultilinetextbox=1
76886     end if 
76888   fnend 
76890 ! 
76892   def fnsetcurserposition$*255(spec$*255,position;___,attrpos,endpos)
76894     let endpos=attrpos=pos(spec$,",",pos(spec$,",",pos(spec$,",")+1)+1)+1
76896     do while pos("0123456789",spec$(endpos:endpos))
76898       let endpos+=1
76900     loop 
76902     let endpos-=1
76904     let spec$(attrpos:endpos)=str$(position)
76906     let fnsetcurserposition$=spec$
76908   fnend 
76910 ! 
76912 FINDCHANGES: ! Examine two arrays and find the differences, adding them to a third array
76914   def fnfindchanges(mat s_data$,mat s_subs,mat save_s_data$,mat save_s_subs,mat changed_s_data$,mat changed_s_subs;___,index,x,z)
76916     for index=1 to udim(mat s_subs)
76918       let x=srch(mat save_s_subs,s_subs(index))
76920       if x<1 or trim$(save_s_data$(x))<>trim$(s_data$(index)) then 
76922         let z=udim(mat changed_s_data$)+1
76924         mat changed_s_data$(z)
76926         mat changed_s_subs(z)
76928 ! 
76930         let changed_s_data$(z)=s_data$(index)
76932         let changed_s_subs(z)=s_subs(index)
76934       end if 
76936     next index
76938   fnend 
76940 ! 
76942 ! 
76944 UNIQUENAME: ! Return a unique name for the control by adding the count
76946   def fnuniquename$*54(mat controlname$,control;___,index,enumerator,count)
76948     for index=1 to udim(mat controlname$)
76950       if lwrc$(trim$(controlname$(index)))=lwrc$(trim$(controlname$(control))) then 
76952         let count+=1
76954         if index<=control then 
76956           let enumerator+=1
76958         end if 
76960       end if 
76962     next index
76964     if count>1 or trim$(controlname$(control))="" then 
76966       let fnuniquename$=trim$(controlname$(control))&str$(enumerator)
76968     else 
76970       let fnuniquename$=trim$(controlname$(control))
76972     end if 
76974   fnend 
76976   def library fngetuniquename$*54(mat controlname$,control)=fnuniquename$(mat controlname$,control)
76978 ! 
76980   dim rlv_subs(1)
76982   dim rlv_readdata$(1)*1000
76984   dim rlv_savereaddata$(1)*1000
76986   dim listviewrecords(1)
76988 ! 
76990 READLISTVIEWS: ! This Function Reads The Listview On The Screen If There Is One.
76992   def fnreadlistviews(function,mat f$, mat f, &currentkey$,&currentrec,mat selectedkeys$,mat selectedrecords,mat s_data,mat s_spec$,listviewindex,mat old_data,wwindow,fdatafile,mat form$, mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,&nextrow;___,listviewspec$*40,index,anythingchanged,multiselect,numberofcolumns)
76994 ! 
76996     if listviewindex and listviewindex<=udim(mat multiselect) and ~protected(listviewindex) and ~invisible(listviewindex) then ! If There's a listview
76998       let listviewspec$=s_spec$(udim(mat s_spec$))
77000       let listviewspec$=listviewspec$(1:pos(listviewspec$,",",-1)-1)
77002       let listviewspec$=listviewspec$(1:pos(listviewspec$,",",-1)-1)
77004       let numberofcolumns=fncountcolumns(parent$(listviewindex),mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)+1
77006 ! 
77008 ! .      ! If its multiselect then read mat SelectedKeys$ and mat SelectedRecords
77010       if multiselect(listviewindex) then 
77012 ! 
77014 ! .         ! Read the selected records from the listview
77016         mat rlv_readdata$(s_data(1)*numberofcolumns)
77018         input #wwindow, fields listviewspec$&",ROW,SEL,NOWAIT" : mat rlv_readdata$
77020 ! 
77022 ! .         ! $$$$$ Discuss with Gordon, why is this necessary?
77024 ! .         !  This is happening because when nothing is selected, s_data(1) has 40 in it. Easy to reproduce, just unselect everything
77026 ! .         !  on expense listview.
77028 ! .         !  The ROWCNT,SEL is returning 40 but the ROW,SEL,NOWAIT is returning 0 elements.
77030 ! .         ! For now, the following code should hopefully keep it from bombing.
77032 ! .         ! Seems like ROWCNT,SEL returns ROWSUB,SELONE for last thing selected if nothing is selected.
77034 ! 
77036 ! .         ! if udim(mat Rlv_ReadData$)<S_Data(1)*NumberOfColumns then
77038 ! .         !    let S_Data(1)=int(udim(mat Rlv_ReadData$)/NumberOfColumns)
77040 ! .         ! end if
77042 ! 
77044 ! .         ! Read the keys array from the data file
77046         if ~fnsameas(mat rlv_savereaddata$,mat rlv_readdata$) then 
77048           mat rlv_savereaddata$(udim(mat rlv_readdata$))=rlv_readdata$
77050           mat selectedkeys$(s_data(1))
77052           mat selectedrecords(s_data(1))
77054 ! 
77056           if fdatafile and file(fdatafile)>=0 then 
77058             for index=1 to udim(mat selectedrecords)
77060               let selectedrecords(index)=val(rlv_readdata$((index*numberofcolumns)))
77062               let selectedkeys$(index)=fnreadcurrentkey$(selectedrecords(index),fdatafile,mat f$,mat f,mat form$,mat screenio$)
77064             next index
77066           else 
77068             mat rlv_subs(s_data(1))
77070             input #wwindow, fields listviewspec$&",ROWSUB,SEL,NOWAIT" : mat rlv_subs
77072             for index=1 to udim(mat selectedrecords)
77074               let selectedrecords(index)=val(rlv_readdata$((index*numberofcolumns)))
77076               let selectedkeys$(index)=str$(rlv_subs(index))
77078             next index
77080           end if 
77082           let anythingchanged=1
77084         end if 
77086 ! 
77088         let multiselect=1
77090       end if 
77092 ! 
77094       if fn42 then ! Use fn42+ NEXT for greater control over the listview
77096         input #wwindow, fields listviewspec$&",ROWSUB,NEXT,NOWAIT" : s_data(1)
77098       else 
77100         input #wwindow, fields listviewspec$&",ROWSUB,CUR,NOWAIT" : s_data(1)
77102       end if 
77104 ! 
77106 ! .      ! Read CurrentKey$
77108       if ~fnsamea(mat old_data,mat s_data) then 
77110         if fdatafile and file(fdatafile)>=0 then 
77112           mat rlv_readdata$(numberofcolumns)
77114           if fn42 then 
77116             input #wwindow, fields listviewspec$&",ROW,NEXT,NOWAIT" : mat rlv_readdata$
77118           else 
77120             input #wwindow, fields listviewspec$&",ROW,CUR,NOWAIT" : mat rlv_readdata$
77122           end if 
77124           let currentkey$=fnreadcurrentkey$(val(rlv_readdata$(numberofcolumns)),fdatafile,mat f$,mat f,mat form$,mat screenio$)
77126           let currentrec=val(rlv_readdata$(numberofcolumns))
77128         else 
77130           let currentkey$=str$(s_data(1))
77132           let currentrec=s_data(1)
77134         end if 
77136 ! 
77138         let anythingchanged=1
77140         mat old_data(udim(mat s_data))=s_data
77142 ! 
77144         if ~multiselect then 
77146           mat selectedkeys$(1) : mat selectedrecords(1)
77148           let selectedkeys$(1)=currentkey$
77150           let selectedrecords(1)=currentrec
77152         end if 
77154       end if 
77156 ! 
77158       if anythingchanged then 
77160 ! .         ! Here is where we link to our READ event for Listviews
77162         if len(trim$(screenio$(si_readfn))) then 
77164           let fnexecute(screenio$(si_readfn))
77166         end if 
77168       end if 
77170     end if 
77172   fnend 
77174 ! 
77176 READCURRENTKEY: ! Read The Current Record And Build Currentkey$
77178   def fnreadcurrentkey$*255(record,fdatafile,mat f$,mat f,mat form$,mat screenio$)
77180     if record then 
77182 ! .      ! Read new record
77184       read #fdatafile, using form$(fdatafile), rec=record, release: mat f$, mat f norec ignore
77186       let fnreadcurrentkey$=fnbuildkey$(trim$(screenio$(si_filelay)),mat f$,mat f,screenio(si_returnindex))
77188     end if 
77190   fnend 
77192 ! 
77194 REQUIRESGUI: ! Returns True If Any Control Requires Gui
77196   def fnrequiresgui(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
77198     let fnrequiresgui=1 ! Non-Gui Compatability Doesn't work yet
77200   fnend 
77202 ! 
77204 INITIALIZEMATS: ! Initialize The Mat S Array For Dumb (Mute) Screen Controls
77206   def fninitializemats(mat s$,mat screensubs$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index)
77208     mat s$(0)
77210     mat screensubs$(0)
77212 ! 
77214     for index=1 to udim(mat controlname$)
77216       if lwrc$(trim$(fieldtype$(index))) = "caption" or lwrc$(trim$(fieldtype$(index))) = "button" or lwrc$(trim$(fieldtype$(index))) = "p" or lwrc$(trim$(fieldtype$(index))) = "combo" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "caption" # "button" # "p" # "combo"
77218         if trim$(fieldname$(index))="" then 
77220           if len(trim$(controlname$(index))) then 
77222             if srch(mat screensubs$,lwrc$(trim$(controlname$(index))))<1 then 
77224               mat s$(udim(mat s$)+1)
77226               mat screensubs$(udim(mat s$))
77228               let s$(udim(mat s$))=fncalculatedata$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,1)
77230               let screensubs$(udim(mat s$))=lwrc$(trim$(controlname$(index)))
77232             end if 
77234           end if 
77236         end if 
77238       else if lwrc$(trim$(fieldtype$(index))) = "c" or lwrc$(trim$(fieldtype$(index))) = "listchld" or lwrc$(trim$(fieldtype$(index))) = "check" or lwrc$(trim$(fieldtype$(index))) = "search" or lwrc$(trim$(fieldtype$(index))) = "filter" then ! #Case# "c" # "listchld" # "check" # "search" # "filter"
77240         if trim$(fieldname$(index))="" then 
77242           if len(trim$(controlname$(index))) then 
77244             if srch(mat screensubs$,lwrc$(trim$(controlname$(index))))<1 then 
77246               mat s$(udim(mat s$)+1)
77248               mat screensubs$(udim(mat s$))
77250               let screensubs$(udim(mat s$))=lwrc$(trim$(controlname$(index)))
77252             end if 
77254           end if 
77256         end if 
77258       end if  ! #End Select#
77260     next index
77262   fnend 
77264 ! 
77266   dim oldinvisible
77268   dim erasespec$(1)*255, erasedata$(1)
77270   def fneraseinvisible(window,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,jndex)
77272     mat erasespec$(udim(mat fieldtype$))
77274     mat oldinvisible(udim(mat invisible))
77276 ! 
77278     for index=1 to udim(mat fieldtype$)
77280       if invisible(index) and ~oldinvisible(index) then ! If its invisible and it used to not be
77282         if fnisinput(fieldtype$(index)) then ! We only care about input controls, output are handled elsewhere.
77284           let jndex+=1
77286           let erasespec$(jndex)=str$(vposition(index))&","&str$(hposition(index))&",C 1"
77288         end if 
77290       end if 
77292     next index
77294 ! 
77296     mat oldinvisible=invisible
77298 ! 
77300     if jndex then ! If anything found, then erase it.
77302       mat erasespec$(jndex)
77304       mat erasedata$(jndex)
77306       print #window, fields mat erasespec$ : mat erasedata$
77308     end if 
77310   fnend 
77312 ! 
77314   dim blankdata$(1)
77316   def fnblankoldsfields$(mat spec$,mat oldspec$,window;___,index,outdex,h,v,w,t$,oh,ov,ow,ot$)
77318     if udim(mat spec$)=udim(mat oldspec$) then 
77320       for index=1 to udim(mat spec$)
77322 ! 
77324         let fnparsespec(oldspec$(index),oh,ov,ow,ot$)
77326         let fnparsespec(spec$(index),h,v,w,t$)
77328 ! 
77330         if uprc$(t$)="C" and uprc$(ot$)="C" and (oh<>h or ov<>v or ow>w) then 
77332 ! .            ! Save it
77334           let outdex+=1
77336           let oldspec$(outdex)=oldspec$(index)(1:pos(oldspec$(index),",",pos(oldspec$(index),",",pos(oldspec$(index),",")+1)+1)-1)&",/W:W"
77338         end if 
77340       next index
77342       mat oldspec$(outdex)
77344     end if 
77346     for index=1 to udim(mat oldspec$)
77348       let oldspec$(index)=oldspec$(index)(1:pos(oldspec$(index),",",pos(oldspec$(index),",",pos(oldspec$(index),",")+1)+1)-1)&",/W:W"
77350     next index
77352 ! 
77354     if udim(mat oldspec$) then 
77356       mat blankdata$(udim(mat oldspec$))=("")
77358       print #wwindow, fields mat oldspec$ : mat blankdata$
77360     end if 
77362   fnend 
77364 ! 
77366 !  $$$$$ The following code doesn't support Radio Buttons or Listviews
77368   def fnparsespec(spec$*255,&h,&v,&w,&type$)
77370     let h=v=w=0
77372     let v=val(spec$(1:pos(spec$,",")-1)) conv ignore
77374     let spec$=spec$(pos(spec$,",")+1:len(spec$))
77376     let h=val(spec$(1:pos(spec$,",")-1)) conv ignore
77378     let spec$=spec$(pos(spec$,",")+1:len(spec$))
77380     let spec$=spec$(pos(spec$,"/")+1:len(spec$))
77382     let type$=spec$(1:1)
77384     if pos(spec$,",",3) then let spec$=spec$(3:pos(spec$,",",3))
77386     let w=val(spec$) conv ignore
77388   fnend 
77390 ! 
77392   dim tempfilename$*1023
77394 GENERATEOUTPUTSPECS: ! Generate The Specs For Output Only Controls
77396   def fngenerateoutputspecs(window,mat s_data$,mat s_spec$,mat s_help$,mat s$,mat screensubs$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$;forceindex,___,index,outdex,subindex,position)
77398     mat s_data$(0) : mat s_spec$(0) : mat s_help$(0)
77400     mat controlspec$(udim(mat controlname$))
77402     for index=1 to udim(mat controlname$)
77404       if ~invisible(index) then 
77406         if lwrc$(trim$(fieldtype$(index))) = "caption" or lwrc$(trim$(fieldtype$(index))) = "button" or lwrc$(trim$(fieldtype$(index))) = "p" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "caption" # "button" # "p"
77408           let outdex=udim(mat s_data$)+1
77410           mat s_data$(outdex)
77412           mat s_spec$(outdex)
77414           mat s_help$(outdex)
77416 ! 
77418 ! .            ! Read from mat S$ if its S$, or F$ and F if its them. If its neither, set it from description.
77420           if trim$(fieldname$(index))="" and trim$(controlname$(index))<>"" and (subindex:=srch(mat screensubs$,lwrc$(trim$(controlname$(index)))))>0 then 
77422             let s_data$(outdex)=s$(subindex)
77424           else if trim$(fieldname$(index))><"" then 
77426             if fnisnumber(mat fieldsssubs$,mat fieldsnsubs$,fieldname$(index)) then 
77428               let position=srch(mat fieldsnsubs$,lwrc$(trim$(fieldname$(index))))
77430               if position>0 then 
77432                 let s_data$(outdex)=str$(f(position))
77434               end if 
77436             else 
77438               let position=srch(mat fieldsssubs$,lwrc$(trim$(fieldname$(index))))
77440               if position>0 then 
77442                 let s_data$(outdex)=f$(position)
77444               end if 
77446             end if 
77448           else 
77450             let s_data$(outdex)=fncalculatedata$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,1)
77452           end if 
77454 ! 
77456 ! .            ! Apply Conversion in if possible
77458           if cnvrtin$(index)(1:1)="{" or cnvrtin$(index)(1:1)="#" then 
77460             let tempdata$=s_data$(outdex)
77462             let fnconvertin(tempdata$,cnvrtin$(index),index,outdex)
77464             if tempdata$<>s_data$(outdex) then let s_data$(outdex)=tempdata$
77466           else if len(trim$(cnvrtin$(index))) then 
77468             let s_data$(outdex)=cnvrt$(cnvrtin$(index),val(s_data$(outdex))) error ignore
77470           end if 
77472 ! 
77474 ! .            ! After 4.2 and Higher, we now have to check to ensure the image file exists because BR now gives error 760 if it doesnt.
77476 ! .            !  we have to do this in a painful and slow way in order to preserve the case of the file name while stripping off
77478 ! .            !  the optional image parameters that cause the exists function to fail
77480           if lwrc$(trim$(fieldtype$(index)))="p" and fn42 then 
77482             let tempfilename$=s_data$(outdex)
77484             if pos(lwrc$(tempfilename$),":isotropic") then let tempfilename$=tempfilename$(1:pos(lwrc$(tempfilename$),":isotropic")-1)
77486             if pos(lwrc$(tempfilename$),":noresize") then let tempfilename$=tempfilename$(1:pos(lwrc$(tempfilename$),":noresize")-1)
77488             if pos(lwrc$(tempfilename$),":tile") then let tempfilename$=tempfilename$(1:pos(lwrc$(tempfilename$),":tile")-1)
77490             if ~len(trim$(tempfilename$)) or ~exists(tempfilename$) then 
77492               let s_data$(outdex)=setting_imagepath$&"\nothing.gif"
77494             end if 
77496           end if 
77498 ! 
77500           let specwidth(index)=len(s_data$(outdex))
77502 ! 
77504           let s_spec$(outdex)=fncalculatespec$(window,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
77506           if (trim$(function$(index))="" or protected(index)) and lwrc$(trim$(fieldtype$(index)))<>"button" then 
77508             let s_spec$(outdex)=s_spec$(outdex)(1:pos(s_spec$(outdex),",",-1)-1)&",-1"
77510           end if 
77512           if forceindex then 
77514             if lwrc$(trim$(fieldtype$(index)))="button" then 
77516               let s_spec$(outdex)=s_spec$(outdex)(1:pos(s_spec$(outdex),",",-1)-1)&",B"&str$(forceindex)
77518             else 
77520               let s_spec$(outdex)=s_spec$(outdex)(1:pos(s_spec$(outdex),",",-1)-1)&","&str$(forceindex)
77522             end if 
77524           end if 
77526 ! 
77528           let s_help$(outdex)=fncalculatehelp$(tooltip$(index))
77530           let controlspec$(index)=s_spec$(outdex)
77532 ! 
77534         else if lwrc$(trim$(fieldtype$(index))) = "c" or lwrc$(trim$(fieldtype$(index))) = "search" or lwrc$(trim$(fieldtype$(index))) = "combo" or lwrc$(trim$(fieldtype$(index))) = "filter" then ! #Case# "c" # "search" # "combo" # "filter"
77536           if protected(index) then ! Show Input Controls As Output Controls If They Are Protected.
77538             let outdex=udim(mat s_data$)+1
77540             mat s_data$(outdex)
77542             mat s_spec$(outdex)
77544             mat s_help$(outdex)
77546 ! 
77548 ! .               ! Read from mat S$ if its S$, or F$ and F if its them. If its neither, leave it blank.
77550             if trim$(fieldname$(index))="" and trim$(controlname$(index))<>"" and (subindex:=srch(mat screensubs$,lwrc$(trim$(controlname$(index)))))>0 then 
77552               let s_data$(outdex)=s$(subindex)
77554             else if trim$(fieldname$(index))><"" then 
77556               if fnisnumber(mat fieldsssubs$,mat fieldsnsubs$,fieldname$(index)) then 
77558                 let position=srch(mat fieldsnsubs$,lwrc$(trim$(fieldname$(index))))
77560                 if position>0 then 
77562                   let s_data$(outdex)=str$(f(position))
77564                 end if 
77566               else 
77568                 let position=srch(mat fieldsssubs$,lwrc$(trim$(fieldname$(index))))
77570                 if position>0 then 
77572                   let s_data$(outdex)=f$(position)
77574                 end if 
77576               end if 
77578             else 
77580               let s_data$(outdex)=""
77582             end if 
77584 ! 
77586 ! .               ! Apply Conversion in if possible
77588             if cnvrtin$(index)(1:1)="{" or cnvrtin$(index)(1:1)="#" then 
77590               let tempdata$=s_data$(outdex)
77592               let fnconvertin(tempdata$,cnvrtin$(index),index,outdex)
77594               if tempdata$<>s_data$(outdex) then let s_data$(outdex)=tempdata$
77596             else if len(trim$(cnvrtin$(index))) then 
77598               let s_data$(outdex)=cnvrt$(cnvrtin$(index),val(s_data$(outdex))) error ignore
77600             end if 
77602 ! .!
77604             let s_spec$(outdex)=fncalculatespec$(window,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
77606             let s_help$(outdex)=fncalculatehelp$(tooltip$(index))
77608             if forceindex then 
77610               let s_spec$(outdex)=s_spec$(outdex)(1:pos(s_spec$(outdex),",",-1)-1)&","&str$(forceindex)
77612             end if 
77614             let controlspec$(index)=s_spec$(outdex)
77616           end if 
77618 ! .!
77620         else if lwrc$(trim$(fieldtype$(index))) = "check" then ! #Case# "check"
77622           if protected(index) then ! Show Input Controls As Output Controls If They Are Protected.
77624             let outdex=udim(mat s_data$)+1
77626             mat s_data$(outdex)
77628             mat s_spec$(outdex)
77630             mat s_help$(outdex)
77632 ! .   !
77634             if trim$(fieldname$(index))="" and trim$(controlname$(index))<>"" and (subindex:=srch(mat screensubs$,lwrc$(trim$(controlname$(index)))))>0 then 
77636               let s_data$(outdex)=fninchecked$(s$(subindex),truevalue$(index))&fncalculatedata$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,1)
77638               let specwidth(index)=len(s_data$(outdex))+1 ! (1 For The Checkmark)
77640             else 
77642               if fnisnumber(mat fieldsssubs$,mat fieldsnsubs$,fieldname$(index)) then 
77644                 let position=srch(mat fieldsnsubs$,lwrc$(trim$(fieldname$(index))))
77646                 if position>0 then 
77648                   let s_data$(outdex)=fninchecked$(str$(f(position)),truevalue$(index))&fncalculatedata$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,1)
77650                 end if 
77652               else 
77654                 let position=srch(mat fieldsssubs$,lwrc$(trim$(fieldname$(index))))
77656                 if position>0 then 
77658                   let s_data$(outdex)=fninchecked$(f$(position),truevalue$(index))&fncalculatedata$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,1)
77660                 end if 
77662               end if 
77664             end if 
77666 ! .   !
77668             let s_spec$(outdex)=fncalculatespec$(window,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
77670             let s_help$(outdex)=fncalculatehelp$(tooltip$(index))
77672             if forceindex then 
77674               let s_spec$(outdex)=s_spec$(outdex)(1:pos(s_spec$(outdex),",",-1)-1)&","&str$(forceindex)
77676             end if 
77678             let controlspec$(index)=s_spec$(outdex)
77680           end if 
77682 ! 
77684         else ! #Case Else#
77686 ! .           ! Ignore all non-protected or Invisible controls
77688 ! 
77690         end if  ! #End Select#
77692       end if 
77694     next index
77696   fnend 
77698 ! 
77700   def library fnisoutputspec(type$)=fnisoutput(type$)
77702   def fnisoutput(type$)
77704     if lwrc$(trim$(type$)) = "caption" or lwrc$(trim$(type$)) = "button" or lwrc$(trim$(type$)) = "p" or lwrc$(trim$(type$)) = "frame" or lwrc$(trim$(type$)) = "screen" then ! #Select# lwrc$(trim$(type$)) #Case# "caption" # "button" # "p" # "frame" # "screen"
77706       let fnisoutput=1
77708     else ! #Case Else#
77710       let fnisoutput=0
77712     end if  ! #End Select#
77714   fnend 
77716 ! 
77718   def library fnisinputspec(type$)=fnisinput(type$)
77720   def fnisinput(type$)
77722     if lwrc$(trim$(type$)) = "c" or lwrc$(trim$(type$)) = "search" or lwrc$(trim$(type$)) = "check" or lwrc$(trim$(type$)) = "combo" or lwrc$(trim$(type$)) = "filter" then ! #Select# lwrc$(trim$(type$)) #Case# "c" # "search" # "check" # "combo" # "filter"
77724       let fnisinput=1
77726     else ! #Case Else#
77728       let fnisinput=0
77730     end if  ! #End Select#
77732   fnend 
77734 ! 
77736 GENERATEINPUTSPECS: ! Generate The Specs For Input From All Io Controls
77738   def fngenerateinputspecs(window,mat s_data$,mat s_data,mat s_spec$,mat s_subs,mat s_help$,mat s_old$,mat f$,mat f,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,&listviewindex,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,mat controlspec$;forceindex,mat otherchanged_s_subs,___,index,outdex,subindex,position)
77740 ! 
77742     mat s_data$(0) : mat s_spec$(0) : mat s_subs(0) : mat s_data(0) : mat s_help$(0)
77744     mat controlspec$(udim(mat controlname$))
77746 ! 
77748     for index=1 to udim(mat controlname$) ! Loop Once For Regular Fields
77750       if ~protected(index) and ~invisible(index) then 
77752         if lwrc$(trim$(fieldtype$(index))) = "c" or lwrc$(trim$(fieldtype$(index))) = "search" or lwrc$(trim$(fieldtype$(index))) = "combo" or lwrc$(trim$(fieldtype$(index))) = "filter" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "c" # "search" # "combo" # "filter"
77754           let outdex=udim(mat s_data$)+1
77756           mat s_data$(outdex)
77758           mat s_spec$(outdex)
77760           mat s_subs(outdex)
77762           mat s_help$(outdex)
77764 ! .!
77766           if trim$(fieldname$(index))="" and trim$(controlname$(index))<>"" and (subindex:=srch(mat screensubs$,lwrc$(trim$(controlname$(index)))))>0 then 
77768             let s_data$(outdex)=s$(subindex)
77770           else 
77772             if fnisnumber(mat fieldsssubs$,mat fieldsnsubs$,fieldname$(index)) then 
77774               let position=srch(mat fieldsnsubs$,lwrc$(trim$(fieldname$(index))))
77776               if position>0 then 
77778                 let s_data$(outdex)=str$(f(position))
77780               end if 
77782             else 
77784               let position=srch(mat fieldsssubs$,lwrc$(trim$(fieldname$(index))))
77786               if position>0 then 
77788                 let s_data$(outdex)=f$(position)
77790               end if 
77792             end if 
77794           end if 
77796 ! 
77798 ! .            ! Apply Conversion if possible...
77800           if cnvrtin$(index)(1:1)="{" or cnvrtin$(index)(1:1)="#" then 
77802             let tempdata$=s_data$(outdex)
77804             let fnconvertin(tempdata$,cnvrtin$(index),index,outdex)
77806             if tempdata$<>s_data$(outdex) then let s_data$(outdex)=tempdata$
77808           else if len(trim$(cnvrtin$(index))) then 
77810             let s_data$(outdex)=cnvrt$(cnvrtin$(index),val(s_data$(outdex))) error ignore
77812           end if 
77814 ! 
77816 ! 
77818           let s_spec$(outdex)=fncalculatespec$(window,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,0,(srch(mat otherchanged_s_subs,index)>0))
77820           let s_subs(outdex)=index
77822           let s_help$(outdex)=fncalculatehelp$(tooltip$(index))
77824           if forceindex then 
77826             let s_spec$(outdex)=s_spec$(outdex)(1:pos(s_spec$(outdex),",",-1)-1)&","&str$(forceindex)
77828           end if 
77830           let controlspec$(index)=s_spec$(outdex)
77832 ! .!
77834         else if lwrc$(trim$(fieldtype$(index))) = "check" then ! #Case# "check"
77836           let outdex=udim(mat s_data$)+1
77838           mat s_data$(outdex)
77840           mat s_spec$(outdex)
77842           mat s_subs(outdex)
77844           mat s_help$(outdex)
77846 ! .!
77848           if trim$(fieldname$(index))="" and trim$(controlname$(index))<>"" and (subindex:=srch(mat screensubs$,lwrc$(trim$(controlname$(index)))))>0 then 
77850             let s_data$(outdex)=fninchecked$(s$(subindex),truevalue$(index))&fncalculatedata$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,1)
77852             let specwidth(index)=len(s_data$(outdex))+1 ! (1 For The Checkmark)
77854           else 
77856             if fnisnumber(mat fieldsssubs$,mat fieldsnsubs$,fieldname$(index)) then 
77858               let position=srch(mat fieldsnsubs$,lwrc$(trim$(fieldname$(index))))
77860               if position>0 then 
77862                 let s_data$(outdex)=fninchecked$(str$(f(position)),truevalue$(index))&fncalculatedata$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,1)
77864               end if 
77866             else 
77868               let position=srch(mat fieldsssubs$,lwrc$(trim$(fieldname$(index))))
77870               if position>0 then 
77872                 let s_data$(outdex)=fninchecked$(f$(position),truevalue$(index))&fncalculatedata$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,1)
77874               end if 
77876             end if 
77878           end if 
77880 ! .!
77882           let s_spec$(outdex)=fncalculatespec$(window,index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,0,0,(srch(mat otherchanged_s_subs,index)>0))
77884           let s_subs(outdex)=index
77886           let s_help$(outdex)=fncalculatehelp$(tooltip$(index))
77888           if forceindex then 
77890             let s_spec$(outdex)=s_spec$(outdex)(1:pos(s_spec$(outdex),",",-1)-1)&","&str$(forceindex)
77892           end if 
77894 ! 
77896           let controlspec$(index)=s_spec$(outdex)
77898 ! .!
77900         else ! #Case Else#
77902 ! .! .         Ignore All Output Controls
77904 ! .!
77906         end if  ! #End Select#
77908       end if 
77910     next index
77912     for index=1 to udim(mat controlname$) ! Loop Again For Listviews - They Must Come Last
77914       if ~protected(index) and ~invisible(index) then 
77916         if lwrc$(trim$(fieldtype$(index))) = "listview" then ! #Select# Lwrc$(Trim$(Fieldtype$(Index))) #Case# "listview"
77918           mat s_data(udim(mat s_data)+1)
77920           mat s_spec$(udim(mat s_spec$)+1)
77922 ! 
77924           let listviewindex=index
77926           let s_spec$(udim(mat s_spec$))=fncalculatelistviewspec$(index,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
77928           if multiselect(index) then 
77930             let s_spec$(udim(mat s_spec$))=s_spec$(udim(mat s_spec$))&",ROWCNT,SEL"
77932           else 
77934             let s_spec$(udim(mat s_spec$))=s_spec$(udim(mat s_spec$))&",ROWSUB,SELONE"
77936           end if 
77938           let controlspec$(index)=s_spec$(udim(mat s_spec$))
77940 ! 
77942         else ! #Case Else#
77944 ! .            Ignore All other Controls
77946 ! 
77948         end if  ! #End Select#
77950       end if 
77952     next index
77954     mat s_old$(udim(mat s_data$))=s_data$
77956   fnend 
77958 ! 
77960   def fninchecked$(test$*255,truevalue$*255)
77962     if trim$(test$)=trim$(truevalue$) then 
77964       let fninchecked$="^"
77966     end if 
77968   fnend 
77970 ! 
77972   def fnoutchecked$(test$*255,truevalue$*255,falsevalue$*255,&return$)
77974     if test$(1:1)="^" then 
77976       let return$=truevalue$
77978     else 
77980       if lwrc$(falsevalue$)<>"~ignore~" then 
77982         let return$=falsevalue$
77984       end if 
77986     end if 
77988   fnend 
77990 ! 
77992   dim tempdata$*8000
77994 ! 
77996 VALIDATEANDSAVE: ! Validate And Save The Controls
77998   def fnvalidateandsave(mat s_data$,mat s_spec$,mat s_subs,mat s_old$,mat f$,mat f,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,index,position,subindex,return$*255,number$*255)
78000     for index=1 to udim(mat s_subs)
78002       if ~protected(s_subs(index)) then ! Should Never Be Protected Because Protected Is Never Added To The Input In The First Place.
78004         if trim$(s_old$(index))<>trim$(s_data$(index)) then 
78006           if lwrc$(trim$(fieldtype$(s_subs(index)))) = "c" or lwrc$(trim$(fieldtype$(s_subs(index)))) = "search" or lwrc$(trim$(fieldtype$(s_subs(index)))) = "combo" or lwrc$(trim$(fieldtype$(s_subs(index)))) = "filter" then ! #Select# Lwrc$(Trim$(Fieldtype$(S_Subs(Index)))) #Case# "c" # "search" # "combo" # "filter"
78008 ! .   ! .         ! First apply screencontrol validation to data value
78010             let tempdata$=s_data$(index)
78012             if trim$(function$(s_subs(index)))="" or fnvalidatecontrol(tempdata$,function$(s_subs(index)),index,s_subs(index)) then 
78014               if tempdata$<>s_data$(index) then let s_data$(index)=tempdata$
78016               if trim$(fieldname$(s_subs(index)))="" and trim$(controlname$(s_subs(index)))<>"" and (subindex:=srch(mat screensubs$,lwrc$(trim$(controlname$(s_subs(index))))))>0 then 
78018                 if uprc$(cnvrtin$(s_subs(index))(1:5))="DATE(" and pos(cnvrtin$(s_subs(index)),")")>0 then 
78020                   let number=fncheckdays(s_data$(index),cnvrtin$(s_subs(index)))
78022                   if number then 
78024                     let s$(subindex)=str$(number)
78026                   end if 
78028                 else 
78030                   let number$=s_data$(index)
78032                   if uprc$(cnvrtin$(s_subs(index))(1:4))="PIC(" and pos(cnvrtin$(s_subs(index)),")")>0 then 
78034 ! .                           ! If its pic, we want to ignore all the insertion characters when we turn it back into a number.
78036                     let number$=fnuncnvrt$(number$,cnvrtin$(s_subs(index)))
78038                   end if 
78040                   let s$(subindex)=number$
78042                 end if 
78044               else 
78046                 if fnisnumber(mat fieldsssubs$,mat fieldsnsubs$,fieldname$(s_subs(index))) then 
78048                   let position=srch(mat fieldsnsubs$,lwrc$(trim$(fieldname$(s_subs(index)))))
78050                   if position>0 then 
78052                     if uprc$(cnvrtin$(s_subs(index))(1:5))="DATE(" and pos(cnvrtin$(s_subs(index)),")")>0 then 
78054                       let number=fncheckdays(s_data$(index),cnvrtin$(s_subs(index)))
78056                       if number then 
78058                         let f(position)=number
78060                       end if 
78062                     else 
78064                       let number$=s_data$(index)
78066                       if uprc$(cnvrtin$(s_subs(index))(1:4))="PIC(" and pos(cnvrtin$(s_subs(index)),")")>0 then 
78068 ! .                                 ! If its pic, we want to ignore all the insertion characters when we turn it back into a number.
78070                         let number$=fnuncnvrt$(number$,cnvrtin$(s_subs(index)))
78072                       end if 
78074 ! 
78076                       let number=val(number$) conv IGNORENUMBER
78078                       let f(position)=number
78080 IGNORENUMBER: ! Jump here when bad number entered
78082                     end if 
78084                   end if 
78086                 else 
78088                   let position=srch(mat fieldsssubs$,lwrc$(trim$(fieldname$(s_subs(index)))))
78090                   if position>0 then 
78092                     let f$(position)=s_data$(index)
78094                   end if 
78096                 end if 
78098               end if 
78100             else 
78102               if tempdata$<>s_data$(index) then let s_data$(index)=tempdata$
78104             end if 
78106           else if lwrc$(trim$(fieldtype$(s_subs(index)))) = "check" then ! #Case# "check"
78108             let tempdata$=s_data$(index)
78110             if trim$(function$(s_subs(index)))="" or fnvalidatecontrol(tempdata$,function$(s_subs(index)),index,s_subs(index)) then 
78112               if tempdata$<>s_data$(index) then let s_data$(index)=tempdata$
78114               if trim$(fieldname$(s_subs(index)))="" and trim$(controlname$(s_subs(index)))<>"" and (subindex:=srch(mat screensubs$,lwrc$(trim$(controlname$(s_subs(index))))))>0 then 
78116                 let fnoutchecked$(s_data$(index),truevalue$(s_subs(index)),falsevalue$(s_subs(index)),s$(subindex))
78118               else 
78120 ! 
78122                 if fnisnumber(mat fieldsssubs$,mat fieldsnsubs$,fieldname$(s_subs(index))) then 
78124                   let position=srch(mat fieldsnsubs$,lwrc$(trim$(fieldname$(s_subs(index)))))
78126                   if position>0 then 
78128                     let return$=str$(f(position))
78130                     let fnoutchecked$(s_data$(index),truevalue$(s_subs(index)),falsevalue$(s_subs(index)),return$)
78132                     let f(position)=fnreturnvalue(return$)
78134                   end if 
78136                 else 
78138                   let position=srch(mat fieldsssubs$,lwrc$(trim$(fieldname$(s_subs(index)))))
78140                   if position>0 then 
78142                     let fnoutchecked$(s_data$(index),truevalue$(s_subs(index)),falsevalue$(s_subs(index)),f$(position))
78144                   end if 
78146                 end if 
78148               end if 
78150             else 
78152               if tempdata$<>s_data$(index) then let s_data$(index)=tempdata$
78154             end if 
78156 ! .   !
78158           else ! #Case Else#
78160 ! .   ! .         Ignore All Output Controls
78162 ! .   !
78164           end if  ! #End Select#
78166         end if 
78168       end if 
78170     next index
78172   fnend 
78174 ! 
78176   def fnuncnvrt$*255(number$*255,picspec$*255;___,checkfor$*255,containsb,containsd,containss,containspm,index)
78178     if pos(uprc$(checkfor$),"B") then let containsb=1
78180     if pos(uprc$(checkfor$),"D") then let containsd=1
78182 ! .   ! if pos(uprc$(CheckFor$),"$") then let ContainsS=1
78184 ! .   ! if pos(uprc$(CheckFor$),"-") then let ContainsPM=1
78186 ! .   ! if pos(uprc$(CheckFor$),"+") then let ContainsPM=1
78188 ! 
78190     let checkfor$=picspec$(5:pos(picspec$,")")-1)
78192     let checkfor$=srep$(checkfor$,"Z","")
78194     let checkfor$=srep$(checkfor$,"z","")
78196     let checkfor$=srep$(checkfor$,".","")
78198     let checkfor$=srep$(checkfor$,"#","")
78200     let checkfor$=srep$(checkfor$,"-","")
78202     let checkfor$=srep$(checkfor$,"^","")
78204     let checkfor$=srep$(checkfor$,"B","")
78206     let checkfor$=srep$(checkfor$,"b","")
78208     let checkfor$=srep$(checkfor$,"D","")
78210     let checkfor$=srep$(checkfor$,"d","")
78212     let checkfor$=trim$(checkfor$)
78214     let number$=trim$(number$)
78216 ! 
78218     for index=1 to len(checkfor$)
78220       let number$=srep$(number$,checkfor$(index:index),"")
78222     next index
78224 ! 
78226     if containsb then let number$=srep$(number$," ","")
78228     if containsd then let number$=srep$(number$,"-","")
78230 ! 
78232 ! .   ! Right now we're handling $,+,- in another way, but this other way has the problem mentioned below.
78234 ! .   ! if ContainsS then let Number$=srep$(Number$,"$","")
78236 ! .   ! if ContainsPM then
78238 ! .   !    if pos(Number$,"-") then
78240 ! .   !       let Number$="-"&srep$(Number$,"-","")
78242 ! .   !    end if
78244 ! .   !    let Number$=srep$(Number$,"+","")
78246 ! .   ! end if
78248 ! .   ! $$$$$ The above code won't work when pic forces display of a trailing sign. This logic will still think the trailing sign was an operator error.
78250 ! 
78252     let fnuncnvrt$=number$
78254   fnend 
78256 ! 
78258 ! 
78260 CONVERTIN: ! Call Custom Conversion Functions
78262   def fnconvertin(&fieldtext$,function$*255;controlindex,fieldindex)
78264     let fnconvertin=fnexecute(function$,controlindex,fieldtext$,fieldindex)
78266   fnend 
78268 ! 
78270 VALIDATECONTROL: ! Validate A Control Based On Its Validation Routine Or Function Routine
78272   def fnvalidatecontrol(&data$,validatefunction$*255;fieldindex,controlindex)
78274     if validatefunction$(1:1)="|" then 
78276 ! .      ! Preform Predefined Validation Routines Here
78278     else 
78280       let fnvalidatecontrol=fnexecute(validatefunction$,controlindex,data$,fieldindex)
78282     end if 
78284   fnend 
78286 ! 
78288 PASSFILTER: ! Test To See If This Record Passes The Filter For A Listview
78290   def fnpassfilter$(mat f$, mat f,filterfunction$*255;controlindex)
78292     if len(trim$(filterfunction$)) then 
78294       let fnpassfilter$=fnexecute$(filterfunction$,controlindex)
78296     else 
78298       let fnpassfilter$="1"
78300     end if 
78302   fnend 
78304 ! 
78306 ISNUMBER: ! Returns True If Control Is A Number
78308   def fnisnumber(mat fieldsssubs$,mat fieldsnsubs$,fieldname$*50)
78310     if srch(mat fieldsnsubs$,lwrc$(trim$(fieldname$)))>0 then 
78312       let fnisnumber=1
78314     end if 
78316   fnend 
78318 ! 
78320 ! ************************************************
78322 ! *   Date Parser Routine - BR's is too picky    *
78324 ! ************************************************
78326 ! 
78328   def library fndays(string$*255;datespec$*255)
78330     let fndays=fncheckdays(string$,datespec$)
78332   fnend 
78334 ! 
78336   dim number(3)
78338   dim format(3)
78340   dim order(3)
78342   dim dates(3)
78344   dim f_day, f_month, f_year
78346 ! 
78348   def fncheckdays(string$*255;datespec$*255,___,month,day,year,delimit$,i,number,failed,forceyear,tempspec$*255,temp$*255,tempnum,tempmonth,tempyear) ! Function to extract the date value out of a text file
78350 ! .   ! Thank you CLS for the idea to do relative days with + and -
78352     if string$(1:1) = "-" then ! #Select# String$(1:1) #Case# "-"
78354       let temp$=trim$(lwrc$(string$))
78356       let tempnum=-123456789
78358       let tempnum=val(temp$(2:len(temp$)-1)) conv ignore
78360       if tempnum><-123456789 then 
78362         let temp$=temp$(len(temp$):len(temp$))
78364 ! 
78366         if temp$="w" then 
78368           let fncheckdays=days(date$("mm/dd/ccyy"),"mm/dd/ccyy")-(tempnum*7) conv ignore
78370         else if temp$="y" then 
78372           let fncheckdays=days(date$("mm/dd")&"/"&str$(val(date$("ccyy"))-tempnum),"mm/dd/ccyy") conv ignore
78374         else if temp$="m" then 
78376           let tempyear=val(date$("ccyy"))
78378 ! 
78380           let tempmonth=val(date$("mm"))
78382           let tempmonth-=tempnum
78384           if tempmonth<1 then let tempmonth+=12 : let tempyear-=1
78386 ! 
78388           let fncheckdays=days(cnvrt$("PIC(##)",tempmonth)&"/"&date$("dd")&"/"&str$(tempyear),"mm/dd/ccyy") conv ignore
78390         else if temp$="d" then 
78392           let fncheckdays=days(date$("mm/dd/ccyy"),"mm/dd/ccyy")-tempnum conv ignore
78394         else 
78396           let fncheckdays=days(date$("mm/dd/ccyy"),"mm/dd/ccyy")-val(string$(2:999)) conv ignore
78398         end if 
78400       end if 
78402     else if string$(1:1) = "+" then ! #Case# "+"
78404       let temp$=trim$(lwrc$(string$))
78406       let tempnum=-123456789
78408       let tempnum=val(temp$(2:len(temp$)-1)) conv ignore
78410       if tempnum><-123456789 then 
78412         let temp$=temp$(len(temp$):len(temp$))
78414 ! 
78416         if temp$="w" then 
78418           let fncheckdays=days(date$("mm/dd/ccyy"),"mm/dd/ccyy")+(tempnum*7) conv ignore
78420         else if temp$="y" then 
78422           let fncheckdays=days(date$("mm/dd")&"/"&str$(val(date$("ccyy"))+tempnum),"mm/dd/ccyy") conv ignore
78424         else if temp$="m" then 
78426           let tempyear=val(date$("ccyy"))
78428 ! 
78430           let tempmonth=val(date$("mm"))
78432           let tempmonth+=tempnum
78434           if tempmonth>12 then let tempmonth-=12 : let tempyear+=1
78436 ! 
78438           let fncheckdays=days(cnvrt$("PIC(##)",tempmonth)&"/"&date$("dd")&"/"&str$(tempyear),"mm/dd/ccyy") conv ignore
78440         else if temp$="d" then 
78442           let fncheckdays=days(date$("mm/dd/ccyy"),"mm/dd/ccyy")+tempnum conv ignore
78444         else 
78446           let fncheckdays=days(date$("mm/dd/ccyy"),"mm/dd/ccyy")+val(string$(2:999)) conv ignore
78448         end if 
78450       end if 
78452     else ! #Case Else#
78454       let f_day=1
78456       let f_month=2
78458       let f_year=3
78460 ! 
78462       let not_read=-1
78464       let not_given=-2
78466 ! 
78468       mat number(3)
78470       mat order(3)
78472 ! 
78474 ! .      ! Detect order of date spec
78476       if pos(datespec$,"(") and pos(datespec$,")") then 
78478         let datespec$=uprc$(datespec$(pos(datespec$,"(")+1:pos(datespec$,")")-1)) ! Trim out Parens
78480         let tempspec$=srep$(datespec$,"DAY","") ! Ignore Day Spec
78482         let format(f_month)=pos(tempspec$,"M")
78484         let format(f_day)=pos(tempspec$,"D")
78486         let format(f_year)=pos(tempspec$,"Y")
78488       else 
78490 ! .         ! default to month, day, year
78492         let format(f_month)=1
78494         let format(f_day)=4
78496         let format(f_year)=7
78498       end if 
78500 ! 
78502 ! .      ! Sort them to get relative order
78504       mat order=aidx(format)
78506 ! 
78508 ! .      ! Consolidate them, we don't care about ones that weren't there
78510       let fnconsolidateblanks(mat order,mat format)
78512 ! 
78514       mat dates=(not_given)
78516 ! 
78518 ! .      ! Detect Delimiter used, if any
78520       if pos(string$,"/") then 
78522         let delimit$="/"
78524       else if pos(string$,"-") then 
78526         let delimit$="-"
78528       else if pos(string$,",") then 
78530         let delimit$=","
78532       else if pos(string$,".") then 
78534         let delimit$="."
78536       end if 
78538 ! 
78540 ! .      ! Are there delimiters?
78542       if pos(string$,delimit$) then ! There are. So check if theres a year
78544 ! 
78546         let string$=trim$(string$)
78548         mat number=(not_read)
78550 ! 
78552         if pos(string$,delimit$,pos(string$,delimit$)+1) then 
78554 ! .            ! There are three delimiters
78556           let number(1)=val(string$(1:pos(string$,delimit$)-1)) conv ignore
78558           let number(2)=val(string$(pos(string$,delimit$)+1:pos(string$,delimit$,pos(string$,delimit$)+1)-1)) conv ignore
78560           let number(3)=val(string$(pos(string$,delimit$,pos(string$,delimit$)+1)+1:len(string$))) conv ignore
78562           if number(3)=not_read then 
78564             let number(3)=val(string$(pos(string$,delimit$,pos(string$,delimit$)+1)+1:pos(string$,delimit$,pos(string$,delimit$)+1)+4)) conv ignore
78566           end if 
78568           if number(3)=not_read then 
78570             let number(3)=val(string$(pos(string$,delimit$,pos(string$,delimit$)+1)+1:pos(string$,delimit$,pos(string$,delimit$)+1)+2)) conv ignore
78572           end if 
78574           if number(3)=not_read then 
78576             let number(3)=val(string$(pos(string$,delimit$,pos(string$,delimit$)+1)+1:pos(string$,delimit$,pos(string$,delimit$)+1)+1)) conv ignore
78578           end if 
78580         else 
78582 ! .            ! There are only 2 values
78584           let number(1)=val(string$(1:pos(string$,delimit$)-1)) conv ignore
78586           let number(2)=val(string$(pos(string$,delimit$)+1:len(string$))) conv ignore
78588           if number(2)=not_read then 
78590             let number(2)=val(string$(pos(string$,delimit$)+1:pos(string$,delimit$)+4)) conv ignore
78592           end if 
78594           if number(2)=not_read then 
78596             let number(2)=val(string$(pos(string$,delimit$)+1:pos(string$,delimit$)+2)) conv ignore
78598           end if 
78600           if number(2)=not_read then 
78602             let number(2)=val(string$(pos(string$,delimit$)+1:pos(string$,delimit$)+1)) conv ignore
78604           end if 
78606           let number(3)=not_given
78608         end if 
78610 ! 
78612 ! .         ! Here, we go from mat Number to Year Month and Day
78614         for i=1 to udim(mat number)
78616           if number(i)=not_read then 
78618             let failed=1
78620           end if 
78622           if number(i)>1800 then 
78624             let forceyear=i
78626           end if 
78628         next i
78630 ! 
78632         if ~failed then 
78634           if forceyear then 
78636 ! .               ! if any of them are greater then 1800 then its a year
78638 ! .               ! so ignore order given and suck year up as a year
78640             let dates(f_year)=number(forceyear)
78642             let fnconsolidatedirect(mat number,forceyear)
78644             let fnconsolidatechild(mat order,f_year)
78646           end if 
78648 ! 
78650 ! .            ! take the remaining values from their given places
78652           for i=1 to udim(mat order)
78654             let dates(order(i))=number(i)
78656           next i
78658         end if 
78660       else 
78662 ! .         ! There are no delimiters, so do it by position
78664         let number=not_read
78666         let string$=trim$(string$(1:8))
78668         let number=val(string$) conv ignore
78670         if number=not_read then 
78672           let failed=1
78674         end if 
78676 ! 
78678         if ~failed then 
78680           if mod(len(string$),2) then let string$="0"&string$ ! Make string an even length
78682           if len(string$)=2 then ! 1 number given
78684             if udim(order)>1 then ! if there's more then one needed
78686               let failed=1 ! we failed
78688             else ! otherwise use the number
78690               let dates(order(1))=val(string$)
78692             end if 
78694           else if len(string$)=4 then ! 2 nums or a year
78696             if udim(order)=1 and order(1)=f_year then ! if we only gave them a year
78698               let dates(f_year)=val(string$) ! then its a year
78700             else if udim(order)=2 then ! if we gave them 2 numbers
78702               let dates(order(1))=val(string$(1:2))
78704               let dates(order(2))=val(string$(3:4))
78706             else 
78708               let failed=1
78710             end if 
78712           else if len(string$)=6 then ! 3 nums or 1 nums and a year
78714             if udim(order)=2 and (order(1)=f_year or order(2)=f_year) then ! 1 num and 1 year
78716               if order(1)=f_year then 
78718                 let dates(order(1))=val(string$(1:4))
78720                 let dates(order(2))=val(string$(5:6))
78722               else if order(2)=f_year then 
78724                 let dates(order(1))=val(string$(1:2))
78726                 let dates(order(2))=val(string$(3:6))
78728               end if 
78730             else if udim(order)=3 then 
78732               let dates(order(1))=val(string$(1:2))
78734               let dates(order(2))=val(string$(3:4))
78736               let dates(order(3))=val(string$(5:6))
78738             else 
78740               let failed=1
78742             end if 
78744           else if len(string$)=8 then 
78746             if udim(mat order)=3 then 
78748               if order(1)=f_year then 
78750                 let dates(order(1))=val(string$(1:4))
78752                 let dates(order(2))=val(string$(5:6))
78754                 let dates(order(3))=val(string$(7:8))
78756               else if order(2)=f_year then 
78758                 let dates(order(1))=val(string$(1:2))
78760                 let dates(order(2))=val(string$(3:6))
78762                 let dates(order(3))=val(string$(7:8))
78764               else if order(3)=f_year then 
78766                 let dates(order(1))=val(string$(1:2))
78768                 let dates(order(2))=val(string$(3:4))
78770                 let dates(order(3))=val(string$(5:8))
78772               end if 
78774             else 
78776               let failed=1
78778             end if 
78780           else 
78782             let failed=1
78784           end if 
78786         end if 
78788       end if 
78790 ! 
78792       if ~failed then 
78794 ! .         ! if Day or month aren't given, default them
78796         if dates(f_month)<1 then let dates(f_month)=val(date$("mm"))
78798         if dates(f_day)<1 then let dates(f_day)=1
78800 ! 
78802         if dates(f_year)=not_given then 
78804 ! .            ! if they didn't give a year, then use current year
78806           let dates(f_year)=val(date$("ccyy"))
78808         else if dates(f_year)>=0 and dates(f_year)<100 then 
78810 ! .            ! if they gave a 2 digit year then use BR's internal function to turn it into a 4 digit year
78812           let dates(f_year)=val(date$(days(cnvrt$("pic(##)",dates(f_year)),"yy"),"ccyy"))
78814         end if 
78816 ! 
78818         let fncheckdays=days(cnvrt$("pic(##)",dates(f_month))&"/"&cnvrt$("pic(##)",dates(f_day))&"/"&cnvrt$("pic(####)",dates(f_year)),"mm/dd/ccyy") conv ignore
78820       else 
78822         let fncheckdays=days(string$,datespec$) error ignore
78824       end if 
78826     end if  ! #End Select#
78828   fnend 
78830 ! 
78832   def fnconsolidateblanks(mat order,mat format;___,i,j)
78834     for i=1 to udim(mat order)
78836       if format(order(i)) then 
78838         let order(i-j)=order(i)
78840       else 
78842         let j+=1
78844       end if 
78846     next i
78848     mat order((i-1)-j)
78850   fnend 
78852   def fnconsolidatedirect(mat array,number;___,i,j)
78854     for i=1 to udim(mat array)
78856       if i<>number then 
78858         let array(i-j)=array(i)
78860       else 
78862         let j+=1
78864       end if 
78866     next i
78868     mat array((i-1)-j)
78870   fnend 
78872   def fnconsolidatedirects(mat array$,number;___,i,j)
78874     for i=1 to udim(mat array$)
78876       if i<>number then 
78878         let array$(i-j)=array$(i)
78880       else 
78882         let j+=1
78884       end if 
78886     next i
78888     mat array$((i-1)-j)
78890   fnend 
78892   def fnconsolidatechild(mat array,number;___,i,j)
78894     for i=1 to udim(mat array)
78896       if array(i)<>number then 
78898         let array(i-j)=array(i)
78900       else 
78902         let j+=1
78904       end if 
78906     next i
78908     mat array((i-1)-j)
78910   fnend 
78912 ! 
78914   def fnopensticky(index,text$*1000,&window,control,rows,cols,row,col,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;___,r,c,h,w,color$*255,sunken$,spec$*255) ! Open yellow sticky note on the screen
78916     let r=vposition(control)+1
78918     let c=hposition(control)+2
78920     let h=height(control)
78922     let w=width(control)
78924 ! 
78926     let r+=row-1
78928     let c+=col-1
78930 ! 
78932     if r+h-1>rows then let r=rows-h+1
78934     if r<0 then 
78936       let h+=(r-1)
78938       let r=1
78940     end if 
78942     if c+w-1>cols then let c=cols-w+1
78944     if c<0 then 
78946       let w+=(c-1)
78948       let c=1
78950     end if 
78952 ! 
78954     if len(trim$(screenio$(si_mychanges))) then 
78956       let color$=trim$(screenio$(si_mychanges))
78958     else 
78960       let color$="/#000000:#FFFF00"
78962     end if 
78964     if lwrc$(trim$(fieldtype$(control)))="c" or lwrc$(trim$(fieldtype$(control)))="search" or lwrc$(trim$(fieldtype$(control)))="filter" then let sunken$="S"
78966 ! 
78968     open #(window:=fngetfilenumber): "SRow="&str$(r)&",scol="&str$(c)&",rows="&str$(max(1,h))&",cols="&str$(w),display,outin 
78970     let spec$="1,1,"&fncalculatefieldtype$(fieldtype$(control),justify$(control),w,specwidth(control),h,attr$(control))&","&sunken$&color$&","&str$(fnstickybase+index)
78972     print #window, fields spec$ : text$
78974   fnend 
78976 ! 
78978   def fncloseallstickies(mat windows, mat subs, mat data$;___,index)
78980     for index=1 to udim(mat windows)
78982       close #windows(index): 
78984     next index
78986     mat windows(0)
78988     mat subs(0)
78990     mat data$(0)
78992   fnend 
78994 ! 
78996 RESPONDTOUSERACTION: ! Respond To The Users Action Here, Setting Exitmode If Necessary
78998   def fnrespondtouseraction(function,&exitmode,&currentfield,&currentfield$,mat subs,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;window,savedontask,___,keyval$*255,recordval,parent_key$*255,dummy$*255,dummy,loopagain,control)
79000 ! 
79002 ! .   ! Esc Button
79004     if function=99 then 
79006       if ~fnsearchclosely(mat function$,"exitmode","quitonly") then 
79008         if fntheresalistview(mat fieldtype$) then 
79010           let exitmode=quitonly
79012         else 
79014           if savedontask then 
79016             let exitmode=saveandquit
79018           else 
79020             let exitmode=asksaveandquit
79022           end if 
79024         end if 
79026       end if 
79028     end if 
79030 ! 
79032     if function=0 then 
79034       if fntheresalistview(mat fieldtype$) then 
79036         let exitmode=selectandquit
79038       end if 
79040     end if 
79042 ! 
79044     do 
79046       let loopagain=0
79048       if function<fnkeybase and function>fnbase then 
79050 ! .         ! Go back up a level
79052         if fntheresalistview(mat fieldtype$) then 
79054           let exitmode=selectandquit
79056         else 
79058           if savedontask then 
79060             let exitmode=saveandquit
79062           else 
79064             let exitmode=asksaveandquit
79066           end if 
79068         end if 
79070       end if 
79072 ! 
79074       if function>fnkeybase and function<=fnkeybase+udim(mat controlname$) then 
79076         let control=function-fnkeybase
79078         if ~protected(control) and trim$(fnremoveprotectedattribute$(attr$(control)))=trim$(attr$(control)) then 
79080 ! 
79082           if lwrc$(trim$(fieldtype$(control))) = "button" or lwrc$(trim$(fieldtype$(control))) = "caption" or lwrc$(trim$(fieldtype$(control))) = "p" then ! #Select# Lwrc$(Trim$(Fieldtype$(Control))) #Case# "button" # "caption" # "p"
79084             if trim$(function$(control))<>"" then 
79086               let fnexecute(function$(control),control)
79088             end if 
79090           else if lwrc$(trim$(fieldtype$(control))) = "c" then ! #Case# "c"
79092             if (newfield:=srch(mat subs,control))>0 then 
79094               let currentfield=newfield
79096             end if 
79098           else if lwrc$(trim$(fieldtype$(control))) = "search" or lwrc$(trim$(fieldtype$(control))) = "filter" then ! #Case# "search" # "filter"
79100             let exitmode=selectandquit ! This makes "enter" on search box exit
79102 ! 
79104           else if lwrc$(trim$(fieldtype$(control))) = "screen" then ! #Case# "screen"
79106 ! .               ! if trim$(Function$(Control))<>"" then
79108             let dummy$="["&fieldname$(control)&"]"&function$(control)
79110             let fnparsescreeninfo(dummy$,keyval$,parent_key$,dummy,dummy,dummy,dummy,dummy,recordval,parentkey$)
79112             let scr_freeze
79114             let fnmaster$(fieldname$(control),keyval$,vposition(control),hposition(control),parent_key$,window,0,0,recordval,path$)
79116             let scr_freeze
79118             let fnmaster$(fieldname$(control),keyval$,vposition(control),hposition(control),parent_key$,window,1,0,recordval,path$,0,0,function)
79120 ! .               ! end if
79122           else ! #Case Else#
79124 ! .! .            ! Ignore other control types
79126 ! .!
79128           end if  ! #End Select#
79130 ! 
79132           if fkey<>function then 
79134             let function=fkey
79136             let loopagain=1
79138           end if 
79140         end if 
79142       end if 
79144     loop while loopagain
79146 ! 
79148     if function=93 or fkey=93 then 
79150       if fntheresalistview(mat fieldtype$) then 
79152         let exitmode=quitonly
79154       else 
79156         if savedontask then 
79158           let exitmode=saveandquit
79160         else 
79162           let exitmode=asksaveandquit
79164         end if 
79166       end if 
79168     end if 
79170 ! 
79172     if function=92 or fkey=92 then 
79174       if fntheresalistview(mat fieldtype$) then 
79176         let exitmode=selectandquit
79178       else 
79180         let exitmode=saveandquit
79182       end if 
79184     end if 
79186 ! 
79188 ! .   ! Here's where we link to our MainLoop event
79190     if len(trim$(screenio$(si_loopfn))) then 
79192       let fnexecute(screenio$(si_loopfn))
79194     end if 
79196 ! 
79198   fnend 
79200 ! 
79202 ! 
84000 !  #Autonumber# 84000,5
84005 OPENWINDOW: ! Opens A User Screen
84010   def fnopenwindow(vpos,hpos,mat screenio$, mat screenio;parent,displayonly,forcethisindex,active,editing,___,windowattributes$*1024,wwindow,fgcolor$,bgcolor$,forceindex)
84015 ! 
84020     let windowattributes$=fnmakewindowspec$(vpos,hpos,mat screenio$,mat screenio,parent,displayonly,active,editing)
84025     let wwindow:=fngetfilenumber(-1)
84030 ! 
84035     if displayonly then let forceindex=wwindow+1000
84040     if forcethisindex then let forceindex=forcethisindex
84045 ! 
84050     if forceindex and fn42 then 
84055       let windowattributes$ = windowattributes$ & ", Fkey=" & str$(forceindex)
84060     end if 
84065 ! 
84070     open #wwindow: windowattributes$, display, outin 
84075 ! 
84080     if forceindex and ~fn42 then 
84085       print #wwindow, fields "1,1,C "&str$(screenio(si_vsize)*screenio(si_hsize))&",/W:T,"&str$(forceindex) : "" ! Make it hot
84090     end if 
84095 ! 
84100     let fnopenwindow=wwindow
84105 ! 
84110   fnend 
84115 ! 
84120   def fnmakewindowspec$*1024(vpos,hpos,mat screenio$, mat screenio;parent,displayonly,active,editing,___,windowattributes$*1024,fgcolor$,bgcolor$)
84125     if trim$(screenio$(si_picture))<>"" then 
84130       let windowattributes$=", Picture="&screenio$(si_picture)
84135     end if 
84140 ! 
84145     if trim$(screenio$(si_attributes))<>"" then 
84150       if fntestattributes(screenio$(si_attributes),vpos,hpos) then 
84155         let windowattributes$ = windowattributes$ & ", " & screenio$(si_attributes)
84160       end if 
84165     end if 
84170 ! 
84175     if ((active or ~displayonly) and len(trim$(screenio$(si_activecolor))) and fntestattributes(screenio$(si_activecolor),vpos,hpos)) then 
84180       let windowattributes$ = windowattributes$ & ", N=" & screenio$(si_activecolor)
84185     else if active and editing then 
84190       let windowattributes$=windowattributes$&", N=/#000000:#FFFF00"
84195     else if (trim$(screenio$(si_fgcolor))<>"" or trim$(screenio$(si_bgcolor))<>"") and ~pos(uprc$(windowattributes$),"N=") then 
84200       let fgcolor$=trim$(screenio$(si_fgcolor))
84205       let bgcolor$=trim$(screenio$(si_bgcolor))
84210       if fgcolor$="" then let fgcolor$="W"
84215       if bgcolor$="" then let bgcolor$="W"
84220       if len(fgcolor$)=6 then let fgcolor$="#"&fgcolor$
84225       if len(bgcolor$)=6 then let bgcolor$="#"&bgcolor$
84230 ! 
84235       let windowattributes$=windowattributes$&", N=/"&fgcolor$&":"&bgcolor$
84240     end if 
84245 ! 
84250     if screenio(si_border) then 
84255       let windowattributes$ = windowattributes$ & ", Border=S"
84260     end if 
84265 ! 
84270     if trim$(screenio$(si_windcap))<>"" then 
84275       let windowattributes$ = windowattributes$ & ", Caption=" & trim$(screenio$(si_windcap))
84280     end if 
84285 ! 
84290     if parent=-1 then 
84295       if fn42 then let windowattributes$ = windowattributes$ & ", Parent=None"
84300     else if parent then 
84305       let windowattributes$ = windowattributes$ & ", Parent=" & str$(parent)
84310     end if 
84315 ! 
84320     let fnmakewindowspec$="SROW=" & str$(vpos) & ", SCOL=" & str$(hpos) & ", ROWS=" & str$(screenio(si_vsize)) & ", COLS=" & str$(screenio(si_hsize)) & windowattributes$
84325   fnend 
84330 ! 
84335 ! 
84340 ! .! Color Picker
84345   dim colorshades$(38)
84350   def fncolorpicker$(old$;row,column,viewtext$,&function,___,window,index,current$,hcode$,currentspec$*32,hcodespec$,sel,colorrow,colorcol,lastindex,oldhcode$,newindex)
84355 ! 
84360     let scr_freeze
84365     if ~row then let row=14 : let column=38
84370 ! 
84375     mat colorspec$(38) : mat colortext$(38)
84380     mat captionspec$(2) : mat captiontext$(2)
84385 ! 
84390     mat colortext$=("")
84395 ! 
84400     if colorshades$(1)="" then 
84405       for index=1 to udim(mat colorspec$)
84410         let colorshades$(index)=fnbuttoncolor2$(index)
84415       next index
84420     end if 
84425 ! 
84430     let hcodespec$="6,8,8/CU 6,X"
84435     let hcode$=old$
84440     if ~fnvalidhexcolor(hcode$) then let hcode$="W"
84445     let currentspec$="6,19,CC 20,"&fnbuildcolor$(hcode$)&",B14"
84450     let current$=viewtext$
84455     let captiontext$(1)="Color Picker"
84460     let captionspec$(1)="1,3,C 14,[TB]"
84465     let captiontext$(2)="HTML:"
84470     let captionspec$(2)="6,2,CR 5,[TB]"
84475 ! 
84480     open #(window:=fngetfilenumber(100)): "sRow="&str$(row)&",sCol="&str$(column)&",Rows=7,Cols=40,Border=S",display,outin 
84485     print #window, fields mat captionspec$ : mat captiontext$
84490     print #window, fields "4,5,CC 8,/W:W,B7001;4,17,CC 8,/W:W,B7002;4,29,CC 8,/W:W,B7003" : "W","T",""
84495     print #window, fields "1,35,CC 5,/W:W,B8000" : "Reset"
84500     do 
84505       mat colortext$=("")
84510       for index=1 to udim(mat colorspec$)
84515         let colorrow=2
84520         let colorcol=index+1
84525         let colorspec$(index)=str$(colorrow)&","&str$(colorcol)&",C 1,"&colorshades$(index)&",B71"&cnvrt$("pic(##)",index)
84530         if (lwrc$(colorshades$(index))(11:16)=lwrc$(oldhcode$)) then 
84535           let colortext$(index)="*"
84540         end if 
84545       next index
84550       print #window, fields mat colorspec$ : mat colortext$
84555       print #window, fields currentspec$ : current$
84560       rinput #window, fields hcodespec$ : hcode$
84565       let sel=fkey : if sel=0 then let sel=14
84570       if sel>7000 then 
84575         if sel = 7001 then ! #Select# Sel #Case# 7001
84580           let hcode$ = "W"
84585         else if sel = 7002 then ! #Case# 7002
84590           let hcode$ = "T"
84595         else if sel = 7003 then ! #Case# 7003
84600           let hcode$ = ""
84605         else if sel = 8000 then ! #Case# 8000
84610           for index=1 to udim(mat colorspec$)
84615             let colorshades$(index)=fnbuttoncolor2$(index)
84620           next index
84625         else ! #Case Else#
84630           let colorshades$(sel-7100)=fnselectshadecolor$(colorshades$(sel-7100),sel-7100,1+row,(sel-7099)+column) ! Row is 2, Col is Index+1
84635           let hcode$ =colorshades$(sel-7100)(11:16)
84640         end if  ! #End Select#
84645       end if 
84650       if hcode$<>oldhcode$ then ! If Hcode$ Changed Then
84655         if hcode$="" or fnvalidhexcolor(hcode$) then 
84660           let oldhcode$=hcode$
84665         else 
84670           let hcode$=oldhcode$
84675         end if 
84680       end if 
84685       let currentspec$=currentspec$(1:pos(currentspec$,"/")-1)&fnbuildcolor$(hcode$)&",B14"
84690     loop until (sel=14) or (sel=99) or ((sel>1100) and (sel<1699)) or (sel=98) or (sel=93) or (sel=44) or (sel=19) or fkey=93
84695     if sel=99 then 
84700       let fncolorpicker$=old$
84705     else 
84710       let fncolorpicker$=hcode$
84715     end if 
84720     if (sel<>14) and (sel<>99) then let function=sel
84725     close #window: 
84730   fnend 
84735 ! 
84740   dim shadespec$(36)*32,shadetext$(36)
84745 ! 
84750   def fnselectshadecolor$(scode$,index;srow,scol,___,window,row,col,sel,x$,shadeindex,startcol,startrow,shadecolor$)
84755     if len(scode$)=6 then let scode$=fnbuildcolor$(scode$)
84760     if ~srow then let srow=10
84765     if ~scol then let scol=30
84770 ! 
84775     let startcol=6
84780     let startrow=1
84785     mat shadetext$=("")
84790 ! 
84795     for row=1 to 6
84800       for col=1 to 6
84805         let shadeindex=(row-1)*6+col
84810         let shadecolor$=fnshadecolor$(index,row,col)
84815         let shadespec$(shadeindex)=str$(row)&","&str$(col)&",C 1,"&shadecolor$&",B71"&str$(row)&str$(col)
84820         if lwrc$(shadecolor$)=lwrc$(scode$) then 
84825           let shadetext$(shadeindex)="*"
84830           let startcol=col
84835           let startrow=row
84840         end if 
84845       next col
84850     next row
84855 ! 
84860     let scol=max(2,scol-startcol)
84865     let srow=max(2,(srow-startrow)+1)
84870 ! 
84875     open #(window:=fngetfilenumber): "srow="&str$(srow)&",scol="&str$(scol)&",rows=6,cols=6,border=s", display, outin 
84880 ! 
84885     print #window, fields mat shadespec$ : mat shadetext$
84890     let x$=""
84895     rinput #0, fields "2,2,C 1,AEX" : x$
84900     let sel=fkey : if sel=0 then let sel=14
84905     if sel>7100 then 
84910       let scode$=fnshadecolor$(index,val(str$(sel)(3:3)),val(str$(sel)(4:4)))
84915     end if 
84920 ! 
84925     let fnselectshadecolor$=scode$
84930     close #window: 
84935   fnend 
84940 ! 
84945   dim colorspec$(216)*31,colortext$(216)
84950   dim captionspec$(2),captiontext$(2)
84955 ! 
84960 COLORPICKERH: !   ***** Allows The User To Select A Color And Returns Hex
84965   def fncolorpickerh$(old$;row,column,viewtext$,&function,___,window,index,current$,hcode$,currentspec$*32,hcodespec$,sel,colorrow,colorcol,lastindex,oldhcode$,newindex)
84970     let scr_freeze
84975     if ~row then let row=14 : let column=38
84980     mat colorspec$(216) : mat colortext$(216)
84985     mat captionspec$(2) : mat captiontext$(2)
84990 ! 
84995     for index=1 to udim(colorspec$)
85000       let colorrow=int((index-1)/36)+2 !:
            let colorcol=mod(index-1,36)+3 !:
            let colorspec$(index)=str$(colorrow)&","&str$(colorcol)&",C 1,"&fnbuttoncolor$(colorrow,colorcol)&",B7"&str$(colorrow)&cnvrt$("pic(##)",colorcol)
85005       if (fnbuttoncolor$(colorrow,colorcol)(11:16)=old$) then !:
              let colortext$(index)="*" !:
              let lastindex=index !:
              let oldhcode$=old$ !:
            else !:
              let colortext$(index)=" "
85010     next index
85015     let hcodespec$="11,8,8/CU 6,X"
85020     let hcode$=old$
85025     if ~fnvalidhexcolor(hcode$) then let hcode$="W"
85030     let currentspec$="11,19,CC 20,"&fnbuildcolor$(hcode$)&",B14"
85035     let current$=viewtext$
85040     let captiontext$(1)="Color Picker"
85045     let captionspec$(1)="1,3,C 14,[TB]"
85050     let captiontext$(2)="HTML:"
85055     let captionspec$(2)="11,2,CR 5,[TB]"
85060     open #(window:=fngetfilenumber(100)): "sRow="&str$(row)&",sCol="&str$(column)&",Rows=12,Cols=40,Border=S",display,outin 
85065     print #window, fields mat captionspec$ : mat captiontext$
85070     print #window, fields "9,5,CC 8,,B7001;9,17,CC 8,,B7002;9,29,CC 8,,B7003" : "W","T",""
85075     do 
85080       print #window, fields mat colorspec$ : mat colortext$
85085       print #window, fields currentspec$ : current$
85090       rinput #window, fields hcodespec$ : hcode$
85095       let sel=fkey : if sel=0 then let sel=14
85100       if sel>7000 then 
85105         if sel = 7001 then ! #Select# Sel #Case# 7001
85110           let hcode$ = "W"
85115         else if sel = 7002 then ! #Case# 7002
85120           let hcode$ = "T"
85125         else if sel = 7003 then ! #Case# 7003
85130           let hcode$ = ""
85135         else ! #Case Else#
85140           let hcode$ = fnbuttoncolor$(val(str$(sel)(2:2)),val(str$(sel)(3:4)))(11:16)
85145         end if  ! #End Select#
85150       end if 
85155       if hcode$<>oldhcode$ then ! If Hcode$ Changed Then
85160         if hcode$="" or fnvalidhexcolor(hcode$) then 
85165           if lastindex then let colortext$(lastindex)=" "
85170           let oldhcode$=hcode$
85175           let lastindex=fnsearchclosely(mat colorspec$,":#"&hcode$)
85180           if lastindex then let colortext$(lastindex)="*"
85185         else 
85190           let hcode$=oldhcode$
85195         end if 
85200       end if 
85205       let currentspec$=currentspec$(1:pos(currentspec$,"/")-1)&fnbuildcolor$(hcode$)&",B14"
85210     loop until (sel=14) or (sel=99) or ((sel>1100) and (sel<1699)) or (sel=98) or (sel=93) or (sel=44) or (sel=19)
85215     if sel=99 then 
85220       let fncolorpickerh$=old$
85225     else 
85230       let fncolorpickerh$=hcode$
85235     end if 
85240     if (sel<>14) and (sel<>99) then let function=sel
85245     close #window: 
85250   fnend 
85255 ! 
85260 BUTTONCOLOR: !   ***** Returns Fg/Bg Color 0f A Specific Button
85265   def fnbuttoncolor$(row,col;___,r,g,b)
85270     let r=int((col-3)/6)
85275     let g=mod((col-3),6)
85280     let b=7-row
85285     let fnbuttoncolor$=fnbuildcolor$(fnpattern$(r)&fnpattern$(g)&fnpattern$(b))
85290   fnend 
85295   def fnbuttoncolor2$(index;___,r,g,b)
85300     let fnbuttonrgb(index,r,g,b)
85305     let fnbuttoncolor2$=fnbuildcolor$(fnpattern2$(r)&fnpattern2$(g)&fnpattern2$(b))
85310   fnend 
85315 ! 
85320   def fnshadecolor$(index,row,col;___,r,g,b,brightness,contrast)
85325     let fnbuttonrgb(index,r,g,b)
85330     let contrast=(6-row)/5
85335     let brightness=(col-1)/5
85340 ! 
85345     let r=6-r
85350     let g=6-g
85355     let b=6-b
85360 ! 
85365     let r*=contrast
85370     let g*=contrast
85375     let b*=contrast
85380 ! 
85385     let r=6-r
85390     let g=6-g
85395     let b=6-b
85400 ! 
85405     let r*=brightness
85410     let g*=brightness
85415     let b*=brightness
85420 ! 
85425     let fnshadecolor$=fnbuildcolor$(fnpattern2$(r)&fnpattern2$(g)&fnpattern2$(b))
85430   fnend 
85435 ! 
85440   def fnbuttonrgb(index,&r,&g,&b)
85445     if index<=1 then 
85450       let r=6
85455       let g=6
85460       let b=6
85465     else if index<=2 then 
85470       let r=0
85475       let g=0
85480       let b=0
85485     else if index<=8 then 
85490       let r=6
85495       let g=index-2
85500       let b=0
85505     else if index<=14 then 
85510       let r=14-index
85515       let g=6
85520       let b=0
85525     else if index<=20 then 
85530       let r=0
85535       let g=6
85540       let b=index-14
85545     else if index<=26 then 
85550       let r=0
85555       let g=26-index
85560       let b=6
85565     else if index<=32 then 
85570       let r=index-26
85575       let g=0
85580       let b=6
85585     else if index<=38 then 
85590       let r=6
85595       let g=0
85600       let b=38-index
85605     end if 
85610   fnend 
85615 ! 
85620 ! 
85625 BUILDCOLOR: !   ***** Builds A Forground And Background That Match
85630   def fnbuildcolor$(bkg$;___,number)
85635     if len(trim$(bkg$)) = 6 then ! #Select# Len(Trim$(Bkg$)) #Case# 6
85640       let number=10
85645       let number=val(bkg$(3:3)) conv ignore
85650       if number<9 then 
85655         let fnbuildcolor$="/#FFFFFF:#"&bkg$
85660       else 
85665         let fnbuildcolor$="/#000000:#"&bkg$
85670       end if 
85675     else ! #Case Else#
85680       let fnbuildcolor$="/W:W"
85685     end if  ! #End Select#
85690   fnend 
85695 ! 
85700 PATTERN: !   ****** Returns A Hex Code Pattern Based On Entered Number
85705   def fnpattern$(x)
85710     if x = 0 then ! #Select# X #Case#  0
85715       let fnpattern$="00"
85720     else if x = 1 then ! #Case#  1
85725       let fnpattern$="33"
85730     else if x = 2 then ! #Case# 2
85735       let fnpattern$="66"
85740     else if x = 3 then ! #Case# 3
85745       let fnpattern$="99"
85750     else if x = 4 then ! #Case# 4
85755       let fnpattern$="CC"
85760     else if x = 5 then ! #Case# 5
85765       let fnpattern$="FF"
85770     end if  ! #End Select#
85775   fnend 
85780 ! 
85785   def fnpattern2$(x)
85790     let x*=255
85795     let x/=6
85800     let fnpattern2$=unhex$(chr$(x))
85805   fnend 
85810 ! 
85815 VALIDHEXCOLOR: !   ***** Returns 1  If Valid Color
85820   def fnvalidhexcolor(hc$;___,index,invalid)
85825     if trim$(hc$) = "W" or trim$(hc$) = "T" then ! #Select# Trim$(Hc$) #Case# "W" # "T"
85830       let fnvalidhexcolor=1
85835     else ! #Case Else#
85840       for index=1 to 6
85845         if ~(pos("0123456789ABCDEF",hc$(index:index))) then let invalid=1
85850       next index
85855       let fnvalidhexcolor=~invalid
85860     end if  ! #End Select#
85865   fnend 
85870 ! 
85875   dim readsettings
85880   def fnsettings
85885     if ~readsettings then 
85890       let fndefaultsettings
85895       if exists("screenio.ini") then 
85900         execute "*subproc screenio.ini"
85905       else if exists("screenio\screenio.ini") then 
85910         execute "*subproc screenio\screenio.ini"
85915       else if exists("\screenio.ini") then 
85920         execute "*subproc \screenio.ini"
85925       else if exists("\screenio\screenio.ini") then 
85930         execute "*subproc \screenio\screenio.ini"
85935       end if 
85940       let readsettings=1
85945 ! 
85950       let fnparsesettings
85955 ! 
85960       library setting_fileiopath$ : fnopenfile, fnclosefile, fngetfilenumber, fnkey$, fnbuildkey$, fnreadlayoutarrays, fndoeslayoutexist, fnreadallkeys, fnreadlayouts, fndisplaylength, fnupdatefile, fnreadnumber, fnreadlayoutpath$, fnreadsubs, fnreadunopenednumber,fnreadunopeneddescription$,fnlog,fnlogarray,fnsetlogchanges,fnlogchanges,fnrunprocfile,fnbuildprocfile,fnreadlockedusers, fnaskcombo$
85965     end if 
85970   fnend 
85975 ! 
85980 EXECUTE: ! Execute Custom Code Call Here
85985   def fnexecute(function$*255;&controlindex,&fieldtext$,&fieldindex,___,pound$)
85990     let pound$="#"
85995     if function$(1:1) = pound$ or function$(1:1) = "{" then ! #Select# Function$(1:1) #Case# Pound$ # "{"
86000       let fnexecute=fnexecutefunction(function$)
86005     else if function$(1:1) = "[" then ! #Case# "["
86010       let fnexecute=fnreturnvalue(fnexecutescreen$(function$))
86015     else if function$(1:1) = "%" then ! #Case# "%"
86020       chain function$(2:len(function$))
86025     else ! #Case Else#
86030       execute function$
86035     end if  ! #End Select#
86040   fnend 
86045   def fnexecute$(function$*255;&controlindex,&fieldtext$,&fieldindex,___,pound$)
86050     let pound$="#"
86055     if function$(1:1) = pound$ or function$(1:1) = "{" then ! #Select# Function$(1:1) #Case# Pound$ # "{"
86060       let fnexecute$=fnexecutefunction$(function$)
86065     else if function$(1:1) = "[" then ! #Case# "["
86070       let fnexecute$=fnexecutescreen$(function$)
86075     else if function$(1:1) = "%" then ! #Case# "%"
86080       chain function$(2:len(function$))
86085     else ! #Case Else#
86090       execute function$
86095     end if  ! #End Select#
86100   fnend 
86105   def fnexecutegetdata(function$*255,mat returndata$;controlindex,fieldtext$,fieldindex)
86110     let pound$="#"
86115     if function$(1:1) = pound$ or function$(1:1) = "{" then ! #Select# Function$(1:1) #Case# Pound$ # "{"
86120       let fnexecutegetdata=fnexecutefunction(function$)
86125     end if  ! #End Select#
86130   fnend 
86135 ! 
86140 CALLSCREEN: ! Use Executescreen To Launch A Screenio Screen. (Use This When Calling Screenio Screens From Code From Within Other Screenio Screens.)
86145   def library fncallscreen$*255(screen$*255;keyval$*255,parent_key$*255,display_only,parent_window,dontredolistview,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$*255,selecting)
86150     if screen$(1:1)<>"[" then let screen$(1:0)="["
86155     if screen$(len(screen$):len(screen$))<>"]" then let screen$=screen$&"]"
86160     let fncallscreen$=fnexecutescreen$(screen$,keyval$,parent_key$,display_only,parent_window,dontredolistview,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$,selecting)
86165   fnend 
86170 ! 
86175 EXECUTESCREEN: ! Push All Arrays Onto Stack And Jump To Another Screen
86180   def fnexecutescreen$*255(screen$*255;keyval$*255,parent_key$*255,display_only,parent_window,dontredolistview,recordval,mat passeddata$,usemyf,mat myf$,mat myf,_path$*255,selecting,___,target_row,target_col,pth$*255)
86185     if _path$<>"" then let pth$=_path$ else let pth$=path$
86190     let fnparsescreeninfo(screen$,keyval$,parent_key$,display_only,parent_window,dontredolistview,target_row,target_col,recordval,parentkey$)
86195     let fnexecutescreen$=fnmasterfm$(screen$,keyval$,target_row,target_col,parent_key$,parent_window,display_only,dontredolistview,recordval,pth$,selecting,0,0,0,mat passeddata$,usemyf,mat myf$,mat myf)
86200   fnend 
86205 ! 
86210   def fnparsescreeninfo(&screen$,&key$,&parentkey$,&displayonly,&parentwindow,&dontredolistview,&targetrow,&targetcol,&record;thisparentkey$*255,___,dividerposition,endscreencode)
86215     let endscreencode=dividerposition=pos(screen$,"(")
86220     if dividerposition < pos(screen$,"]") then 
86225       let targetrow=val(screen$(pos(screen$,"(")+1:pos(screen$,",")-1)) conv ignore
86230       let targetcol=val(screen$(pos(screen$,",")+1:pos(screen$,")")-1)) conv ignore
86235     end if 
86240 ! 
86245     let dividerposition=pos(screen$,"]")
86250 ! 
86255     if endscreencode=0 then 
86260       let endscreencode=dividerposition
86265     else if dividerposition=0 then 
86270       let endscreencode=endscreencode
86275     else 
86280       let endscreencode=min(dividerposition,endscreencode)
86285     end if 
86290 ! 
86295     if dividerposition then 
86300       if trim$(screen$(dividerposition+1:len(screen$)))<>"" then 
86305         if trim$(screen$(dividerposition+1:len(screen$)))(1:1)="{" or trim$(screen$(dividerposition+1:len(screen$)))(1:1)="#" then 
86310           let key$=fnexecutefunction$(trim$(screen$(dividerposition+1:len(screen$))))
86315         else if trim$(screen$(dividerposition+1:len(screen$)))(1:1)="*" then 
86320           execute trim$(screen$(dividerposition+1:len(screen$)))
86325         else 
86330           execute ""&trim$(screen$(dividerposition+1:len(screen$)))
86335         end if 
86340       end if 
86345 ! 
86350       let screen$=uprc$(screen$(2:endscreencode-1))
86355     else if endscreencode then 
86360       let screen$=uprc$(screen$(2:endscreencode-1))
86365     else 
86370       let screen$=uprc$(screen$(2:len(screen$)))
86375     end if 
86380     let screen$=trim$(screen$)
86385   fnend 
86390 ! 
86395 ! .! Clear all screenio arrays in case they've been sullied by a
86400 ! .!  previous screen.
86405   def fnclearallarrays
86410     mat s$(0) : mat f$(0) : mat f(0) : mat fieldsssubs$(0) : mat fieldsnsubs$(0) : mat screensubs$(0) : mat form$(0) : mat s_subs(0) : mat tempdata$(0)
86415     mat temp_s_spec$(0) : mat selectedrecords(0) : mat selectedkeys$(0) : mat save_s$(0) : mat save_f(0) : mat save_f$(0)
86420     mat save_s_subs(0) : mat save_s_data$(0) : mat changed_s_subs(0) : mat changed_s_data$(0) : mat other_s_subs(0) : mat other_s_data$(0)
86425     mat otherchanged_s_subs(0) : mat otherchanged_s_data$(0) : mat read_f(0)
86430     mat read_f$(0) : mat framekeys$(0) : mat framespeccontrol$(0) : mat framewindows(0) : mat screenkeys$(0) : mat screenwindows(0)
86435 ! 
86440     mat controlname$(0) : mat fieldname$(0) : mat function$(0) : mat cnvrtin$(0) : mat specwidth(0)
86445     mat screenio$(0) : mat screenio(0) : mat description$(0) : mat vposition(0) : mat hposition(0) : mat fieldtype$(0) : mat width(0)
86450     mat height(0) : mat truevalue$(0) : mat falsevalue$(0) : mat picture$(0) : mat parent$(0) : mat fgcolor$(0) : mat bgcolor$(0)
86455     mat justify$(0) : mat attr$(0) : mat protected(0) : mat invisible(0) : mat tooltip$(0) : mat cnvrtout$(0) : mat multiselect(0)
86460     mat gridlines(0) : mat userdata$(0) : mat s_old$(0) : mat s_data$(0) : mat s_data(0) : mat s_spec$(0)
86465   fnend 
86470 ! 
90000 ! #Autonumber# 90000,2
90002 STACK: ! Functions To Manage The Stack
90004   dim stack$(1)*255
90006   dim longstack$(1)*2047
90008   dim stack(1)
90010 ! 
90012   dim undostack$(1)*255
90014   dim undolongstack$(1)*2047
90016   dim undostack(1)
90018 ! 
90020   dim size1(1)
90022   dim size2(1)
90024   dim size3(1)
90026 ! 
90028   def fnpushmemory(;___,stringarrays,numberofstringarrays,longstring$*800)
90030 ! .   ! Test how much we're about to push.
90032     let longstring$="mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$,"
90034     let longstring$=longstring$&" mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat MultiSelect,mat UserData$,mat GridLines"
90036     let numberofstringarrays=len(longstring$)-len(srep$(longstring$,"$",""))
90038     let stringarrays=udim(mat controlname$)*numberofstringarrays+udim(mat screenio$)
90040     let stringarrays+=udim(mat screensubs$)+udim(mat s$)+udim(mat form$)+udim(mat f$)
90042     let stringarrays+=udim(mat fieldsssubs$)+udim(mat fieldsnsubs$)+udim(mat temp_s_spec$)
90044     let stringarrays+=udim(mat selectedkeys$)+udim(mat save_s$)+udim(mat save_f$)
90046     let stringarrays+=udim(mat save_s_data$)+udim(mat changed_s_data$)+udim(mat other_s_data$)
90048     let stringarrays+=udim(mat otherchanged_s_data$)+udim(mat read_f$)+udim(mat framekeys$)+udim(mat framespeccontrol$)+udim(screenkeys$)
90050 ! 
90052     if ~fn42 and (udim(mat stack$)+stringarrays>4000 or udim(mat longstack$)>400 or udim(mat stack)>20000) then 
90054       let msgbox("We didn't have enough memory to load this screen. Consider upgrading to BR 4.2 or don't load so many screens at one time.","Out of Memory")
90056       let fnpushmemory=1 ! Out of memory Error
90058     else 
90060       let fnpushdata(0,1)
90062     end if 
90064   fnend 
90066   def fnpopmemory(;dontredolistview)
90068     let fnpopdata
90070 ! 
90072     let fnexecutesetsubscripts(mat screensubs$,"sio_")
90074     let fnexecutesetsubscripts(mat controlname$,"ctl_")
90076     let fnexecuteuniquesubscripts(mat controlname$,"ctl_")
90078 ! 
90080     let fnexecutesetsubscripts(mat fieldsnsubs$,prefix$)
90082     let fnexecutesetsubscripts(mat fieldsssubs$,prefix$)
90084 ! 
90086     let fnestablishlibrarylinkage
90088 ! 
90090     if wwindow=0 or file(wwindow)=-1 then 
90092       let wwindow=fnopenwindow(row,col,mat screenio$,mat screenio)
90094       let fndrawframes(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
90096       let fndrawscreens(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
90098       let fndrawalllistviews(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
90100       let dontredolistview=0 ! Force Redo Listview If Window Has To Be Rebuilt.
90102       let repopulatecombo=1 ! Force Repuplate Combo if Window has to be rebuilt.
90104     end if 
90106 ! 
90108     if ~dontredolistview then 
90110       let lastrow=fnpopulatealllistviews(wwindow,fdatafile,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat form$,mat listviewrecords,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
90112       mat old_data=(0) ! Clear information so it forces read again.
90114       mat rlv_savereaddata$=("")
90116       let s_currentrow=0
90118     end if 
90120   fnend 
90122 ! 
90124 PACK: ! Pack Data Functions
90126 ! 
90128   dim screendata$(1)*255
90130   dim screenlongdata$(1)*2047
90132   dim screendata(1)
90134 ! 
90136   def fnpushdata(;altstack,wipe)
90138     if altstack<>2 then 
90140       let fnpush$(mat passedinf$,altstack,wipe)
90142       let fnpush(mat passedinf,altstack,wipe)
90144       let fnpush$(mat s$,altstack,wipe)
90146       let fnpush$(mat f$,altstack,wipe)
90148       let fnpush(mat f,altstack,wipe)
90150       let fnpush$(mat fieldsssubs$,altstack,wipe)
90152       let fnpush$(mat fieldsnsubs$,altstack,wipe)
90154       let fnpush$(mat screensubs$,altstack,wipe)
90156       let fnpush$(mat form$,altstack,wipe)
90158       let fnpush(mat s_subs,altstack,wipe)
90160       let fnpushvalue$(tempdata$,altstack,wipe)
90162 ! 
90164       let fnpush$(mat temp_s_spec$,altstack,wipe)
90166       let fnpush(mat old_data,altstack,wipe)
90168       let fnpush$(mat rlv_savereaddata$,altstack,wipe)
90170     end if 
90172 ! 
90174     let fnpush(mat selectedrecords,altstack,wipe)
90176     let fnpush$(mat selectedkeys$,altstack,wipe)
90178     let fnpush$(mat save_s$,altstack,wipe)
90180     let fnpush(mat save_f,altstack,wipe)
90182     let fnpush$(mat save_f$,altstack,wipe)
90184 ! 
90186     let fnpush(mat save_s_subs,altstack,wipe)
90188     let fnpush$(mat save_s_data$,altstack,wipe)
90190     let fnpush(mat changed_s_subs,altstack,wipe)
90192     let fnpush$(mat changed_s_data$,altstack,wipe)
90194     let fnpush(mat other_s_subs,altstack,wipe)
90196     let fnpush$(mat other_s_data$,altstack,wipe)
90198     let fnpush(mat otherchanged_s_subs,altstack,wipe)
90200     let fnpush$(mat otherchanged_s_data$,altstack,wipe)
90202     let fnpush(mat read_f,altstack,wipe)
90204     let fnpush$(mat read_f$,altstack,wipe)
90206     let fnpush$(mat controlspec$,altstack,wipe)
90208 ! 
90210     let fnpushscreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,altstack,wipe)
90212   fnend 
90214   def fnpopdata(;altstack)
90216     let fnpopscreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,altstack)
90218 ! 
90220     let fnpop$(mat controlspec$,altstack)
90222     let fnpop$(mat read_f$,altstack)
90224     let fnpop(mat read_f,altstack)
90226     let fnpop$(mat otherchanged_s_data$,altstack)
90228     let fnpop(mat otherchanged_s_subs,altstack)
90230     let fnpop$(mat other_s_data$,altstack)
90232     let fnpop(mat other_s_subs,altstack)
90234     let fnpop$(mat changed_s_data$,altstack)
90236     let fnpop(mat changed_s_subs,altstack)
90238     let fnpop$(mat save_s_data$,altstack)
90240     let fnpop(mat save_s_subs,altstack)
90242 ! 
90244     let fnpop$(mat save_f$,altstack)
90246     let fnpop(mat save_f,altstack)
90248     let fnpop$(mat save_s$,altstack)
90250     let fnpop$(mat selectedkeys$,altstack)
90252     let fnpop(mat selectedrecords,altstack)
90254 ! 
90256     if altstack<>2 then 
90258       let fnpop$(mat rlv_savereaddata$,altstack)
90260       let fnpop(mat old_data,altstack)
90262       let fnpop$(mat temp_s_spec$,altstack)
90264 ! 
90266       let fnpopvalue$(tempdata$,altstack)
90268       let fnpop(mat s_subs,altstack)
90270       let fnpop$(mat form$,altstack)
90272       let fnpop$(mat screensubs$,altstack)
90274       let fnpop$(mat fieldsnsubs$,altstack)
90276       let fnpop$(mat fieldsssubs$,altstack)
90278       let fnpop(mat f,altstack)
90280       let fnpop$(mat f$,altstack)
90282       let fnpop$(mat s$,altstack)
90284       let fnpop(mat passedinf,altstack)
90286       let fnpop$(mat passedinf$,altstack)
90288     end if 
90290   fnend 
90292 ! 
90294   def fnpushscreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;altstack,wipe,___,stack1,stack2,stack3)
90296 ! 
90298     if altstack=1 then 
90300       let stack1=udim(mat undostack$)
90302       let stack2=udim(mat undolongstack$)
90304       let stack3=udim(mat undostack)
90306     end if 
90308 ! 
90310     if altstack<>2 then 
90312       let fnpush$(mat controlname$,altstack,wipe)
90314       let fnpush$(mat fieldname$,altstack,wipe)
90316       let fnpush$(mat function$,altstack,wipe)
90318       let fnpush$(mat cnvrtin$,altstack,wipe)
90320       let fnpush(mat specwidth,altstack,wipe)
90322     end if 
90324 ! 
90326     let fnpush$(mat screenio$,altstack,wipe)
90328     let fnpush(mat screenio,altstack,wipe)
90330     let fnpush$(mat description$,altstack,wipe)
90332     let fnpush(mat vposition,altstack,wipe)
90334     let fnpush(mat hposition,altstack,wipe)
90336     let fnpush$(mat fieldtype$,altstack,wipe)
90338     let fnpush(mat width,altstack,wipe)
90340     let fnpush(mat height,altstack,wipe)
90342     let fnpush$(mat truevalue$,altstack,wipe)
90344     let fnpush$(mat falsevalue$,altstack,wipe)
90346     let fnpush$(mat picture$,altstack,wipe)
90348     let fnpush$(mat parent$,altstack,wipe)
90350     let fnpush$(mat fgcolor$,altstack,wipe)
90352     let fnpush$(mat bgcolor$,altstack,wipe)
90354     let fnpush$(mat justify$,altstack,wipe)
90356     let fnpush$(mat attr$,altstack,wipe)
90358     let fnpush(mat protected,altstack,wipe)
90360     let fnpush(mat invisible,altstack,wipe)
90362     let fnpush$(mat tooltip$,altstack,wipe)
90364     let fnpush$(mat cnvrtout$,altstack,wipe)
90366     let fnpush(mat multiselect,altstack,wipe)
90368     let fnpush(mat gridlines,altstack,wipe)
90370     let fnpush$(mat userdata$,altstack,wipe)
90372     let fnpush$(mat s_old$,altstack,wipe)
90374     let fnpush$(mat s_data$,altstack,wipe)
90376     let fnpush(mat s_data,altstack,wipe)
90378     let fnpush$(mat s_spec$,altstack,wipe)
90380     let fnpush$(mat framekeys$,altstack,wipe)
90382     let fnpush$(mat framespeccontrol$,altstack,wipe)
90384     let fnpush(mat framewindows,altstack,wipe)
90386     let fnpush$(mat screenkeys$,altstack,wipe)
90388     let fnpush(mat screenwindows,altstack,wipe)
90390 ! 
90392     if altstack=1 then 
90394       if udim(mat size1)=0 or udim(mat size2)=0 or udim(mat size3)=0 or size1(udim(mat size1))+size2(udim(mat size2))+size3(udim(mat size3)) then 
90396         mat size1(udim(mat size1)+1)
90398         mat size2(udim(mat size1))
90400         mat size3(udim(mat size1))
90402       end if 
90404 ! 
90406       let size1(udim(mat size1))=udim(mat undostack$)-stack1
90408       let size2(udim(mat size1))=udim(mat undolongstack$)-stack2
90410       let size3(udim(mat size1))=udim(mat undostack)-stack3
90412     end if 
90414   fnend 
90416 ! 
90418   def fnpopscreen(mat screenio$, mat screenio, mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;altstack)
90420     let fnpop(mat screenwindows,altstack)
90422     let fnpop$(mat screenkeys$,altstack)
90424     let fnpop(mat framewindows,altstack)
90426     let fnpop$(mat framespeccontrol$,altstack)
90428     let fnpop$(mat framekeys$,altstack)
90430     let fnpop$(mat s_spec$,altstack)
90432     let fnpop(mat s_data,altstack)
90434     let fnpop$(mat s_data$,altstack)
90436     let fnpop$(mat s_old$,altstack)
90438     let fnpop$(mat userdata$,altstack)
90440     let fnpop(mat gridlines,altstack)
90442     let fnpop(mat multiselect,altstack)
90444     let fnpop$(mat cnvrtout$,altstack)
90446     let fnpop$(mat tooltip$,altstack)
90448     let fnpop(mat invisible,altstack)
90450     let fnpop(mat protected,altstack)
90452     let fnpop$(mat attr$,altstack)
90454     let fnpop$(mat justify$,altstack)
90456     let fnpop$(mat bgcolor$,altstack)
90458     let fnpop$(mat fgcolor$,altstack)
90460     let fnpop$(mat parent$,altstack)
90462     let fnpop$(mat picture$,altstack)
90464     let fnpop$(mat falsevalue$,altstack)
90466     let fnpop$(mat truevalue$,altstack)
90468     let fnpop(mat height,altstack)
90470     let fnpop(mat width,altstack)
90472     let fnpop$(mat fieldtype$,altstack)
90474     let fnpop(mat hposition,altstack)
90476     let fnpop(mat vposition,altstack)
90478     let fnpop$(mat description$,altstack)
90480     let fnpop(mat screenio,altstack)
90482     let fnpop$(mat screenio$,altstack)
90484 ! 
90486     if altstack<>2 then 
90488       let fnpop(mat specwidth,altstack)
90490       let fnpop$(mat cnvrtin$,altstack)
90492       let fnpop$(mat function$,altstack)
90494       let fnpop$(mat fieldname$,altstack)
90496       let fnpop$(mat controlname$,altstack)
90498     end if 
90500 ! 
90502     if altstack=1 then 
90504       mat size1(udim(mat size1)-1)
90506       mat size2(udim(mat size1))
90508       mat size3(udim(mat size1))
90510     end if 
90512   fnend 
90514 ! 
90516   def fnpush$(mat array$;altstack,wipe)
90518     if altstack=1 then 
90520       let fnpush$=fnpusharray$(mat array$,mat undostack$,mat undolongstack$)
90522     else if altstack=2 then 
90524       let fnpush$=fnpusharray$(mat array$,mat screendata$,mat screenlongdata$)
90526     else 
90528       let fnpush$=fnpusharray$(mat array$,mat stack$,mat longstack$)
90530     end if 
90532     if wipe then 
90534       mat array$(0)
90536     end if 
90538   fnend 
90540   def fnpop$(mat array$;altstack)
90542     if altstack=1 then 
90544       let fnpop$=fnpoparray$(mat array$,mat undostack$,mat undolongstack$)
90546     else if altstack=2 then 
90548       let fnpop$=fnpoparray$(mat array$,mat screendata$,mat screenlongdata$)
90550     else 
90552       let fnpop$=fnpoparray$(mat array$,mat stack$,mat longstack$)
90554     end if 
90556   fnend 
90558   def fnpush(mat array;altstack,wipe)
90560     if altstack=1 then 
90562       let fnpush=fnpusharray(mat array,mat undostack)
90564     else if altstack=2 then 
90566       let fnpush=fnpusharray(mat array,mat screendata)
90568     else 
90570       let fnpush=fnpusharray(mat array,mat stack)
90572     end if 
90574     if wipe then 
90576       mat array(0)
90578     end if 
90580   fnend 
90582   def fnpop(mat array;altstack)
90584     if altstack=1 then 
90586       let fnpop=fnpoparray(mat array,mat undostack)
90588     else if altstack=2 then 
90590       let fnpop=fnpoparray(mat array,mat screendata)
90592     else 
90594       let fnpop=fnpoparray(mat array,mat stack)
90596     end if 
90598   fnend 
90600 ! 
90602   dim pushvalue$(1)*2047,pushvalue(1)
90604   def fnpushvalue$(&var$;altstack,wipe)
90606     mat pushvalue$(1)
90608     let pushvalue$(1)=var$
90610     let fnpush$(mat pushvalue$,altstack,wipe)
90612     if wipe then let var$=""
90614   fnend 
90616   def fnpopvalue$(&var$;altstack)
90618     let fnpop$(mat pushvalue$,altstack)
90620     let var$=pushvalue$(1)
90622   fnend 
90624   def fnpushvalue(&var;altstack,wipe)
90626     mat pushvalue(1)
90628     let pushvalue(1)=var
90630     let fnpush(mat pushvalue,altstack,wipe)
90632     if wipe then let var=0
90634   fnend 
90636   def fnpopvalue(&var;altstack)
90638     let fnpop(mat pushvalue,altstack)
90640     let var=pushvalue(1)
90642   fnend 
90644 ! 
90646   dim pushworkarray$(1)*4000
90648   def fnpusharray$(mat array$,mat stack$,mat longstack$;___,startindex,endindex,size,index)
90650     mat pushworkarray$(udim(mat array$))=array$
90652     for index=1 to udim(mat pushworkarray$)
90654       if len(pushworkarray$(index))>255 then 
90656         mat longstack$(udim(mat longstack$)+1)
90658         let longstack$(udim(mat longstack$))=pushworkarray$(index)
90660         let pushworkarray$(index)="[[[loNgsTaCk]]]"
90662       end if 
90664     next index
90666 ! 
90668     let startindex=udim(mat stack$)+1
90670     let size=udim(mat pushworkarray$)
90672     let endindex=startindex+size-1
90674     mat stack$(endindex+1)
90676     if size then 
90678       mat stack$(startindex:endindex)=pushworkarray$
90680     end if 
90682     let stack$(endindex+1)=str$(size)
90684   fnend 
90686   def fnpoparray$(mat array$,mat stack$,mat longstack$;___,startindex,endindex,size,index)
90688     let endindex=udim(mat stack$)-1
90690     let size=val(stack$(endindex+1))
90692     let startindex=endindex-size+1
90694     mat array$(size)
90696     if size then 
90698       mat array$=stack$(startindex:endindex)
90700     end if 
90702     mat stack$(startindex-1)
90704 ! 
90706     for index=udim(mat array$) to 1 step -1
90708       if array$(index)="[[[loNgsTaCk]]]" then 
90710         let array$(index)=longstack$(udim(mat longstack$))
90712         mat longstack$(udim(mat longstack$)-1)
90714       end if 
90716     next index
90718   fnend 
90720   def fnpusharray(mat array,mat stack;___,startindex,endindex,size)
90722     let startindex=udim(mat stack)+1
90724     let size=udim(mat array)
90726     let endindex=startindex+size-1
90728     mat stack(endindex+1)
90730     if size then 
90732       mat stack(startindex:endindex)=array
90734     end if 
90736     let stack(endindex+1)=size
90738   fnend 
90740   def fnpoparray(mat array,mat stack;___,startindex,endindex,size)
90742     let endindex=udim(mat stack)-1
90744     let size=stack(endindex+1)
90746     let startindex=endindex-size+1
90748     mat array(size)
90750     if size then 
90752       mat array=stack(startindex:endindex)
90754     end if 
90756     mat stack(startindex-1)
90758   fnend 
90760 ENDPACK: ! Done with Pack Data Functions
90762 ! 
92000 ! #Autonumber# 92000,5
92005 ! .! ==================================================================
92010 UNDO: ! All The Functions Necessary For Undo Operations Reside Here.
92015 ! .! ==================================================================
92020   def fnundo(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
92025     if udim(mat size1)>1 then 
92030       let fnpushscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) ! Push Onto Redo Stack
92035       let fnpopscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1) ! Pop From Undo Stack
92040       let fnpopscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
92045 ! 
92050       let fnpushscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
92055       let fnpreserveundo(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
92060       let fnundo=1 ! Success
92065     else 
92070       print bell;
92075     end if 
92080   fnend 
92085 ! 
92090   def fnredo(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
92095     if udim(mat stack$)>1 then 
92100       let fnpopscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) ! Pop From Redo Stack
92105       let fnpushscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1) ! Push On Undo Stack
92110       let fnpreserveundo(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
92115       let fnredo=1 ! Success
92120     else 
92125       print bell;
92130     end if 
92135   fnend 
92140 ! 
92145   dim numberofstringarrays
92150 ! 
92155   def fncheckpoint(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines;dontclearredo,___,longstring$*800,purgeamount)
92160     if ~fncheckundo(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines) then 
92165 ! 
92170       if ~numberofstringarrays then 
92175         let longstring$="mat ControlName$,mat FieldName$,mat Description$,mat VPosition,mat HPosition,mat FieldType$,mat SpecWidth,mat Width,mat Height,mat TrueValue$,mat FalseValue$,mat Function$,mat Picture$,mat Parent$,mat FGColor$,mat BGColor$,mat Justify$, mat Attr$,"
92180         let longstring$=longstring$&" mat Protected, mat Invisible, mat Tooltip$, mat CnvrtIn$, mat CnvrtOut$, mat MultiSelect,mat UserData$,mat GridLines"
92185         let numberofstringarrays=len(longstring$)-len(srep$(longstring$,"$",""))
92190       end if 
92195       let purgeamount=udim(mat controlname$)*numberofstringarrays+udim(mat screenio$)+udim(mat framekeys$)+udim(mat framespeccontrol$)+udim(mat windowkeys$)
92200       let fnpurgebackofstack(mat undostack$,mat undolongstack$,mat undostack,purgeamount)
92205 ! 
92210       let fnpushscreen(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1) ! Push On Undo Stack
92215       let fnpreserveundo(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
92220 ! 
92225       if ~dontclearredo then mat stack$(0) : mat longstack$(0) : mat stack(0)
92230     end if 
92235   fnend 
92240 ! 
92245   def fnpurgebackofstack(mat stack$,mat longstack$,mat stack;howmuch,___,index)
92250     do while udim(mat undostack$)>(4000-howmuch) or udim(mat undolongstack$)>400 or udim(mat stack)>20000
92255       let fnpurgefromstack$(mat stack$,mat size1)
92260       let fnpurgefromstack$(mat longstack$,mat size2)
92265       let fnpurgefromstack(mat stack,mat size3)
92270     loop 
92275   fnend 
92280 ! 
92285   def fnpurgefromstack$(mat stack$,mat size;___,index)
92290     for index=size(1)+1 to udim(mat stack$)
92295       let stack$(index-size(1))=stack$(index)
92300     next index
92305     mat stack$(udim(mat stack$)-size(1))
92310 ! 
92315     for index=2 to udim(mat size)
92320       let size(index-1)=size(index)
92325     next index
92330     mat size(udim(mat size)-1)
92335   fnend 
92340 ! 
92345   def fnpurgefromstack(mat stack,mat size;___,index)
92350     for index=size(1)+1 to udim(mat stack)
92355       let stack(index-size(1))=stack(index)
92360     next index
92365     mat stack(udim(mat stack)-size(1))
92370 ! 
92375     for index=2 to udim(mat size)
92380       let size(index-1)=size(index)
92385     next index
92390     mat size(udim(mat size)-1)
92395   fnend 
92400 ! 
92405 ! *************************************************
92410 !    Screen Helper Library Folder Change Upgrade
92415 ! *************************************************
92420   dim _screenio$(1)*255, _screenio
92425   def fncheckhelperlibfolder(;___,screenfile,configfile,sysalreadythere,foundascreen,choice)
92430     if ~exists(setting_screenfolder$) then 
92435       if (2<>msgbox("Your ScreenIO Screen Helper Library Folder is missing or has not been generated yet. Would you like to regenerate it? You will not be able to run any ScreenIO programs until you complete this process.","Update Process: Regenerate Helper Libraries?","Yn","QST")) then 
92440         print "I cannot proceed without this file. Please repair the folder or run the regerate process."
92445         pause  ! Give Programmer Chance To Recover.
92450         stop 
92455       end if 
92460 ! 
92465       execute "mkdir "&setting_screenfolder$
92470 ! 
92475       let choice=msgbox("Would you like me to clean up any unnecessary old Screen Helper Libraries I find? I will move them to the "&setting_screenfolder$&"\archive folder.","Update Process: Clean Up Files?","Yn","QST")
92480       if choice=2 then execute "mkdir "&setting_screenfolder$&"\archive"
92485 ! 
92490 ! .      ! $$$$$ Make sure this update code is Client Server compatable.
92495 ! 
92500       let screenfile=fnopen("screenio",mat _screenio$,mat _screenio,mat form$,1)
92505       if exists(""&setting_screenfolder$&"\screenio.sys") then let sysalreadythere=1
92510       open #(configfile:=fngetfilenumber): "name="&setting_screenfolder$&"\screenio.sys, use", display, output 
92515 ! 
92520       if ~sysalreadythere then 
92525         print #configfile: "rem Your ScreenIO Helper Libraries have been moved into a new folder"
92530         print #configfile: "rem called '"&setting_screenfolder$&"\'. If you referenced any of these files in your"
92535         print #configfile: "rem existing code, you may need to update those references. We have"
92540         print #configfile: "rem generated this brconfig file of substitution statements to aid"
92545         print #configfile: "rem in this process. To use it simply add 'include "&setting_screenfolder$&"\screenio.sys'"
92550         print #configfile: "rem to your brconfig.sys file."
92555         print #configfile: "rem "
92560         print #configfile: "rem WARNING: If any of the following substitution statements conflict with"
92565         print #configfile: "rem one of your file names you will not be able to use them. Carefully"
92570         print #configfile: "rem inspect these files to ensure they do not conflict with the names of"
92575         print #configfile: "rem any of your data files. If they do, you will not be able to use the"
92580         print #configfile: "rem substitution statements because they will affect your data files as"
92585         print #configfile: "rem well as your program name references. You will have to remove those"
92590         print #configfile: "rem lines from this file before using this file, and manually fix all"
92595         print #configfile: "rem the references in your existing code to any programs who's names"
92600         print #configfile: "rem conflict with your data files."
92605         print #configfile: "rem "
92610         print #configfile: "rem If you have any questions about this process, please visit"
92615         print #configfile: "rem http://www.brwiki.com/index.php?title=ScreenIO_Library#Update_Process"
92620         print #configfile: "rem or contact Gabriel Bakker at gabriel.bakker@gmail.com."
92625         print #configfile: 
92630       end if 
92635 ! 
92640       do while file(screenfile)=0
92645         read #screenfile, using form$(screenfile) : mat _screenio$, mat _screenio eof ignore
92650         if file(screenfile)=0 then 
92655           if exists(lwrc$(_screenio$(si_screencode))&".br") then 
92660             let foundascreen=1
92665             if choice=2 then execute "rename "&lwrc$(_screenio$(si_screencode))&".br "&setting_screenfolder$&"\archive\"&lwrc$(_screenio$(si_screencode))&".br"
92670             print #configfile: "substitute "&lwrc$(_screenio$(si_screencode))&" "&setting_screenfolder$&"\"&lwrc$(_screenio$(si_screencode))
92675           end if 
92680           if choice=2 and exists(lwrc$(_screenio$(si_screencode))&".brs") then 
92685             execute "rename "&lwrc$(_screenio$(si_screencode))&".brs "&setting_screenfolder$&"\archive\"&lwrc$(_screenio$(si_screencode))&".brs"
92690           end if 
92695         end if 
92700       loop 
92705 ! .      ! if FoundAScreen then
92710 ! .      !    print #ConfigFile: "setenv ScreenSysIncluded 1"
92715 ! .      ! end if
92720       close #configfile: 
92725       close #screenfile: 
92730       if ~foundascreen and ~sysalreadythere then execute "free "&setting_screenfolder$&"\screenio.sys"
92735       if exists(setting_screenfolder$&"\screenio.sys") then 
92740         let msgbox("If you referenced any of your screenio helper libraries in any of your existing programs, you may have to update those references. Please review this information in notepad while I compile your new helper libraries.","Update Process: screenio.sys Substitution Statements","Ok")
92745         execute "system -c start notepad "&setting_screenfolder$&"\screenio.sys"
92750       end if 
92755       let fnrecompileallscreens(mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
92760     end if 
92765 ! 
92770 ! .   ! UnComment the following code to automatically install screenio.sys
92775 ! .   ! if env$("ScreenSysIncluded")<>"1" then
92780 ! .   !    if exists(setting_ScreenFolder$&"\screenio.sys") then
92785 ! .   !       open #(ConfigFile:=fnGetFileNumber): "name="&fnGetWbCfg$&", use", display, output
92790 ! .   !       print #ConfigFile: "include "&setting_ScreenFolder$&"\screenio.sys"
92795 ! .   !       close #ConfigFile:
92800 ! .   !       execute "config include "&setting_ScreenFolder$&"\screenio.sys"
92805 ! .   !    end if
92810 ! .   ! end if
92815   fnend 
92820 ! 
92825   def fnestablishlibrarylinkage
92830     let fncheckhelperlibfolder
92835     if exists(setting_screenfolder$&"\"&lwrc$(routinename$)&".br") then 
92840       library setting_screenfolder$&"\"&routinename$&".br" : fnfunctionswitch$, fnfunctionswitch, fncheckstringfunction error TRYLIBRARYLINKAGE1
92845       let librarylinkage=1
92850       goto LIBRARYLINKAGEDONE
92855 TRYLIBRARYLINKAGE1: ! Linkage 0 Didn't work, try 1
92860       library setting_screenfolder$&"\"&routinename$&".br" : fnfunctionswitch1$, fnfunctionswitch1, fncheckstringfunction error TRYLIBRARYLINKAGE2
92865       let librarylinkage=2
92870       goto LIBRARYLINKAGEDONE
92875 TRYLIBRARYLINKAGE2: ! Linkage 1 Didn't work, try 2
92880       library setting_screenfolder$&"\"&routinename$&".br" : fnfunctionswitch2$, fnfunctionswitch2, fncheckstringfunction error TRYLIBRARYLINKAGE3
92885       let librarylinkage=3
92890       goto LIBRARYLINKAGEDONE
92895 TRYLIBRARYLINKAGE3: ! Linkage 2 Didn't work, try 3
92900       library setting_screenfolder$&"\"&routinename$&".br" : fnfunctionswitch3$, fnfunctionswitch3, fncheckstringfunction error TRYLIBRARYLINKAGE4
92905       let librarylinkage=4
92910       goto LIBRARYLINKAGEDONE
92915 TRYLIBRARYLINKAGE4: ! Linkage 3 Didn't work, try 4
92920       library setting_screenfolder$&"\"&routinename$&".br" : fnfunctionswitch4$, fnfunctionswitch4, fncheckstringfunction error TRYLIBRARYLINKAGE5
92925       let librarylinkage=5
92930       goto LIBRARYLINKAGEDONE
92935 TRYLIBRARYLINKAGE5: ! Linkage 4 Didn't work, try 5
92940       library setting_screenfolder$&"\"&routinename$&".br" : fnfunctionswitch5$, fnfunctionswitch5, fncheckstringfunction error TRYLIBRARYLINKAGE6
92945       let librarylinkage=6
92950       goto LIBRARYLINKAGEDONE
92955 TRYLIBRARYLINKAGE6: ! Linkage 5 Didn't work, try 6
92960       library setting_screenfolder$&"\"&routinename$&".br" : fnfunctionswitch6$, fnfunctionswitch6, fncheckstringfunction error TRYLIBRARYLINKAGE7
92965       let librarylinkage=7
92970       goto LIBRARYLINKAGEDONE
92975 TRYLIBRARYLINKAGE7: ! Linkage 6 Didn't work, try 7
92980       library setting_screenfolder$&"\"&routinename$&".br" : fnfunctionswitch7$, fnfunctionswitch7, fncheckstringfunction
92985       let librarylinkage=8
92990       goto LIBRARYLINKAGEDONE
92995 LIBRARYLINKAGEDONE: ! Linkage Has Been Set
93000       let subscriptsalreadyset=0 ! We Must Reset All Subscripts When We Come Back From A Window
93005     else 
93010       let librarylinkage=0
93015     end if 
93020   fnend 
93025 ! 
93030   dim subscripts$(1)*800, dataisinside
93035 EXECUTEFUNCTION: ! Execute Custom Function Call
93040   def fnexecutefunction(function$*255;___,repopulatelistviews,redrawlistviews)
93045     if librarylinkage then 
93050       let fngeneratesubscripts(mat subscripts$)
93055 ! 
93060 ! .      ! Pack Screen Data
93065       mat screendata(0)
93070       mat screendata$(0)
93075       mat screenlongdata$(0)
93080 ! 
93085       if function$="{{SetData}}" then 
93090         let fnpushdata(2) ! Pack Screen Data
93095         let dataisinside=1
93100       else if ~dataisinside then 
93105         let fnpushdata(2) ! Pack Screen Data
93110       end if 
93115 ! 
93120       if fnstringfunction(function$) then 
93125         if librarylinkage = 1 then ! #Select# Librarylinkage #Case# 1
93130           let fnexecutefunction=fnreturnvalue(fnfunctionswitch$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93135         else if librarylinkage = 2 then ! #Case# 2
93140           let fnexecutefunction=fnreturnvalue(fnfunctionswitch1$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93145         else if librarylinkage = 3 then ! #Case# 3
93150           let fnexecutefunction=fnreturnvalue(fnfunctionswitch2$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93155         else if librarylinkage = 4 then ! #Case# 4
93160           let fnexecutefunction=fnreturnvalue(fnfunctionswitch3$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93165         else if librarylinkage = 5 then ! #Case# 5
93170           let fnexecutefunction=fnreturnvalue(fnfunctionswitch4$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93175         else if librarylinkage = 6 then ! #Case# 6
93180           let fnexecutefunction=fnreturnvalue(fnfunctionswitch5$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93185         else if librarylinkage = 7 then ! #Case# 7
93190           let fnexecutefunction=fnreturnvalue(fnfunctionswitch6$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93195         else if librarylinkage = 8 then ! #Case# 8
93200           let fnexecutefunction=fnreturnvalue(fnfunctionswitch7$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93205         end if  ! #End Select#
93210       else 
93215         if librarylinkage = 1 then ! #Select# Librarylinkage #Case# 1
93220           let fnexecutefunction=fnfunctionswitch(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93225         else if librarylinkage = 2 then ! #Case# 2
93230           let fnexecutefunction=fnfunctionswitch1(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93235         else if librarylinkage = 3 then ! #Case# 3
93240           let fnexecutefunction=fnfunctionswitch2(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93245         else if librarylinkage = 4 then ! #Case# 4
93250           let fnexecutefunction=fnfunctionswitch3(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93255         else if librarylinkage = 5 then ! #Case# 5
93260           let fnexecutefunction=fnfunctionswitch4(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93265         else if librarylinkage = 6 then ! #Case# 6
93270           let fnexecutefunction=fnfunctionswitch5(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93275         else if librarylinkage = 7 then ! #Case# 7
93280           let fnexecutefunction=fnfunctionswitch6(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93285         else if librarylinkage = 8 then ! #Case# 8
93290           let fnexecutefunction=fnfunctionswitch7(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93295         end if  ! #End Select#
93300       end if 
93305 ! 
93310       if ~dataisinside or function$="{{GetData}}" then 
93315         let fnpopdata(2)
93320         let dataisinside=0
93325       end if 
93330 ! 
93335       if redrawlistviews then 
93340         let fndrawalllistviews(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
93345       end if 
93350       if redrawlistviews or repopulatelistviews then 
93355         let lastrow=fnpopulatealllistviews(wwindow,fdatafile,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat form$,mat listviewrecords,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
93360         mat old_data=(0) ! Clear information so it forces read again.
93365         mat rlv_savereaddata$=("")
93370         let s_currentrow=0
93375       end if 
93380     else 
93385       print "Missing Helper Library"
93390     end if 
93395   fnend 
93400   def fnexecutefunction$*1000(function$*255;___,repopulatelistviews,redrawlistviews)
93405     if librarylinkage then 
93410       let fngeneratesubscripts(mat subscripts$)
93415 ! 
93420 ! .      ! Pack Screen Data
93425       mat screendata(0)
93430       mat screendata$(0)
93435       mat screenlongdata$(0)
93440 ! 
93445       if function$="{{SetData}}" then 
93450         let fnpushdata(2) ! Pack Screen Data
93455         let dataisinside=1
93460       else if ~dataisinside then 
93465         let fnpushdata(2) ! Pack Screen Data
93470       end if 
93475 ! 
93480       if fnstringfunction(function$) then 
93485         if librarylinkage = 1 then ! #Select# Librarylinkage #Case# 1
93490           let fnexecutefunction$=fnfunctionswitch$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93495         else if librarylinkage = 2 then ! #Case# 2
93500           let fnexecutefunction$=fnfunctionswitch1$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93505         else if librarylinkage = 3 then ! #Case# 3
93510           let fnexecutefunction$=fnfunctionswitch2$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93515         else if librarylinkage = 4 then ! #Case# 4
93520           let fnexecutefunction$=fnfunctionswitch3$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93525         else if librarylinkage = 5 then ! #Case# 5
93530           let fnexecutefunction$=fnfunctionswitch4$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93535         else if librarylinkage = 6 then ! #Case# 6
93540           let fnexecutefunction$=fnfunctionswitch5$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93545         else if librarylinkage = 7 then ! #Case# 7
93550           let fnexecutefunction$=fnfunctionswitch6$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93555         else if librarylinkage = 8 then ! #Case# 8
93560           let fnexecutefunction$=fnfunctionswitch7$(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$)
93565         end if  ! #End Select#
93570       else 
93575         if librarylinkage = 1 then ! #Select# Librarylinkage #Case# 1
93580           let fnexecutefunction$=str$(fnfunctionswitch(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93585         else if librarylinkage = 2 then ! #Case# 2
93590           let fnexecutefunction$=str$(fnfunctionswitch1(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93595         else if librarylinkage = 3 then ! #Case# 3
93600           let fnexecutefunction$=str$(fnfunctionswitch2(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93605         else if librarylinkage = 4 then ! #Case# 4
93610           let fnexecutefunction$=str$(fnfunctionswitch3(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93615         else if librarylinkage = 5 then ! #Case# 5
93620           let fnexecutefunction$=str$(fnfunctionswitch4(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93625         else if librarylinkage = 6 then ! #Case# 6
93630           let fnexecutefunction$=str$(fnfunctionswitch5(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93635         else if librarylinkage = 7 then ! #Case# 7
93640           let fnexecutefunction$=str$(fnfunctionswitch6(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93645         else if librarylinkage = 8 then ! #Case# 8
93650           let fnexecutefunction$=str$(fnfunctionswitch7(function$,mat subscripts$,mat passeddata$,fieldindex,fieldtext$,controlindex,key$,exitmode,repopulatelistviews,redrawlistviews,currentfield,currentfield$,prefix$,currentkey$,currentrec,parentkey$,fdatafile,wwindow,currentrow,lockuser$,path$,selecting,mat disk_f$,mat disk_f,onread,trytocontinue,displayonly,active,redrawframes,redrawscreens,repopulatecombo,mat f$,mat f, mat s$,mat cnvrtin$,mat fieldsssubs$,mat fieldsnsubs$,mat fieldname$,mat controlname$,mat screensubs$,mat function$,mat specwidth,mat form$,mat screendata$,mat screenlongdata$,mat screendata,mat returndata$,mat controlspec$))
93655         end if  ! #End Select#
93660       end if 
93665 ! 
93670       if ~dataisinside or function$="{{GetData}}" then 
93675         let fnpopdata(2)
93680         let dataisinside=0
93685       end if 
93690 ! 
93695       if redrawlistviews then 
93700         let fndrawalllistviews(wwindow,mat screenio$,mat screenio,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines,1)
93705       end if 
93710       if redrawlistviews or repopulatelistviews then 
93715         let lastrow=fnpopulatealllistviews(wwindow,fdatafile,mat fieldsssubs$,mat fieldsnsubs$,mat s$,mat screensubs$,mat form$,mat listviewrecords,mat controlname$,mat fieldname$,mat description$,mat vposition,mat hposition,mat fieldtype$,mat specwidth,mat width,mat height,mat truevalue$,mat falsevalue$,mat function$,mat picture$,mat parent$,mat fgcolor$,mat bgcolor$,mat justify$, mat attr$, mat protected, mat invisible, mat tooltip$, mat cnvrtin$, mat cnvrtout$, mat multiselect, mat userdata$, mat gridlines)
93720         mat old_data=(0) ! Clear information so it forces read again.
93725         mat rlv_savereaddata$=("")
93730         let s_currentrow=0
93735       end if 
93740     else 
93745       print "Missing Helper Library"
93750     end if 
93755   fnend 
93760 ! 
93765 GENERATESUBSCRIPTS: ! Generates The Subscripts That Get Passed Into User Defined Functions
93770   def fngeneratesubscripts(mat subscripts$;___,index,subindex)
93775     if ~subscriptsalreadyset then ! If They're already set then leave them alone
93780       mat subscripts$(0)
93785 ! 
93790 ! .      ! fileio subscripts for mat f$ and mat f
93795       let fnaddarraytosubs(mat subscripts$,trim$(prefix$),mat fieldsssubs$)
93800       let fnaddarraytosubs(mat subscripts$,trim$(prefix$),mat fieldsnsubs$)
93805 ! 
93810 ! .      ! fileIO Subscripts for mat ScreenIO$ and mat ScreenIO
93815       let fnaddarraytosubs(mat subscripts$,trim$(screenioprefix$),mat screeniossubs$)
93820       let fnaddarraytosubs(mat subscripts$,trim$(screenioprefix$),mat screenionsubs$)
93825 ! 
93830 ! .      ! screenio subscripts for mat s$ (based on Control Name field)
93835       let fnaddarraytosubs(mat subscripts$,"sio_",mat screensubs$)
93840 ! 
93845 ! .      ! Control Subs based on Control Name field
93850       let fnaddarraytosubs(mat subscripts$,"ctl_",mat controlname$)
93855       let fnadduniquearraytosubs(mat subscripts$,"ctl_",mat controlname$)
93860 ! 
93865 ! .      ! Exitmode Literals such as SaveAndQuit and QuitOnly
93870       let fnaddtosubsarray("let QuitOnly="&str$(quitonly),mat subscripts$)
93875       let fnaddtosubsarray("let SaveAndQuit="&str$(saveandquit),mat subscripts$)
93880       let fnaddtosubsarray("let SelectAndQuit="&str$(selectandquit),mat subscripts$)
93885       let fnaddtosubsarray("let QuitOther="&str$(quitother),mat subscripts$)
93890       let fnaddtosubsarray("let AskSaveAndQuit="&str$(asksaveandquit),mat subscripts$)
93895       let fnaddtosubsarray("let Reload="&str$(reload),mat subscripts$)
93900       let fnaddtosubsarray("let AutoReload="&str$(autoreload),mat subscripts$)
93905       let fnaddtosubsarray("let FkeyPgUp=90",mat subscripts$)
93910       let fnaddtosubsarray("let FkeyPgDn=91",mat subscripts$)
93915       let fnaddtosubsarray("let FkeyUp=102",mat subscripts$)
93920       let fnaddtosubsarray("let FkeyDn=104",mat subscripts$)
93925       let fnaddtosubsarray("let FkeyLeft=103",mat subscripts$)
93930       let fnaddtosubsarray("let FkeyRight=109",mat subscripts$)
93935       let fnaddtosubsarray("let FkeyUpFld=105",mat subscripts$)
93940       let fnaddtosubsarray("let FkeyDnFld=106",mat subscripts$)
93945       let fnaddtosubsarray("let FkeyEsc=99",mat subscripts$)
93950       let fnaddtosubsarray("let FkeyWinClose=93",mat subscripts$)
93955       let fnaddtosubsarray("let FkeyHome=112",mat subscripts$)
93960       let fnaddtosubsarray("let FkeyEnd=113",mat subscripts$)
93965       let fnaddtosubsarray("let FkeyFldPlus=114",mat subscripts$)
93970       let fnaddtosubsarray("let FkeyFldMinus=115",mat subscripts$)
93975       let fnaddtosubsarray("let FkeyClick=201",mat subscripts$)
93980       let fnaddtosubsarray("let FkeyDblClick=202",mat subscripts$)
93985       let fnaddtosubsarray("let FkeyRightClick=100",mat subscripts$)
93990 ! 
93995       let subscriptsalreadyset=1
94000     end if 
94005   fnend 
94010   def fnaddarraytosubs(mat subscripts$,pre$,mat string$;___,index)
94015     for index=1 to udim(mat string$)
94020       if len(trim$(string$(index))) then 
94025         let fnaddtosubsarray("let "&pre$&trim$(string$(index))(1:30-len(trim$(pre$)))&"="&str$(index),mat subscripts$)
94030       end if 
94035     next index
94040   fnend 
94045 ! 
94050 ! 
94055   dim tempsetsubscripts$(1)*800
94060 EXECUTESETSUBSCRIPTS: ! Execute A Subscripts List Setting The Subscripts
94065   def fnexecutesetsubscripts(mat subs$,prefix$;___,index)
94070     if fncheckbeforeexecutesubs(prefix$) then 
94075       mat tempsetsubscripts$(0)
94080       for index=1 to udim(mat subs$)
94085         if len(trim$(subs$(index))) then 
94090           let fnaddtosubsarray("let "&trim$(prefix$)&trim$(subs$(index))(1:30-len(trim$(prefix$)))&"="&str$(index),mat tempsetsubscripts$)
94095         end if 
94100       next index
94105       for index=1 to udim(mat tempsetsubscripts$)
94110         execute tempsetsubscripts$(index)
94115       next index
94120     end if 
94125   fnend 
94130 ! 
94135   dim alreadyknow$(1)*40
94140   dim alreadyknow(1)
94145 ! 
94150   def fncheckbeforeexecutesubs(prefix$;___,index,foundyet)
94155 ! .   ! check arrays to see if we already know
94160     if (index:=srch(mat alreadyknow$,lwrc$(screenio$(si_screencode))&"|&|"&lwrc$(prefix$)))>0 then 
94165       let foundyet=alreadyknow(index)
94170     else 
94175       do 
94180         for index=si_enterfn to si_exitfn
94185           if (foundyet:=fnchecksinglefunction(prefix$,screenio$(index))) then exit do 
94190         next index
94195         for index=1 to udim(mat function$)
94200           if (foundyet:=fnchecksinglefunction(prefix$,function$(index))) then exit do 
94205           if (foundyet:=fnchecksinglefunction(prefix$,cnvrtin$(index))) then exit do 
94210           if (foundyet:=fnchecksinglefunction(prefix$,cnvrtout$(index))) then exit do 
94215         next index
94220       loop while 0
94225       let index=udim(mat alreadyknow$)+1
94230       mat alreadyknow$(index)
94235       mat alreadyknow(index)
94240       let alreadyknow$(index)=lwrc$(screenio$(si_screencode))&"|&|"&lwrc$(prefix$)
94245       let alreadyknow(index)=foundyet
94250     end if 
94255 ! 
94260     let fncheckbeforeexecutesubs=foundyet
94265   fnend 
94270 ! 
94275   def fnchecksinglefunction(prefix$,string$*255)
94280     if ~pos("{#%",trim$(string$)(1:1)) then 
94285       if trim$(string$)(1:1)="[" then let string$(1:pos(string$,"]"))=""
94290       if pos(string$,prefix$) then let fnchecksinglefunction=1
94295     end if 
94300   fnend 
94305 ! 
94310   dim uniquecontrolname$(1)*54
94315   def fnexecuteuniquesubscripts(mat controlname$,prefix$)
94320     let fngetuniquecontrolname(mat uniquecontrolname$,mat controlname$)
94325     let fnexecutesetsubscripts(mat uniquecontrolname$,prefix$)
94330   fnend 
94335 ! 
94340   def fnadduniquearraytosubs(mat subscripts$,prefix$,mat controlname$)
94345     let fngetuniquecontrolname(mat uniquecontrolname$,mat controlname$)
94350     let fnaddarraytosubs(mat subscripts$,prefix$,mat uniquecontrolname$)
94355   fnend 
94360 ! 
94365   def fngetuniquecontrolname(mat uniquecontrolname$,mat controlname$;___,index)
94370     mat uniquecontrolname$(udim(mat controlname$))
94375     for index=1 to udim(mat uniquecontrolname$)
94380       let uniquecontrolname$(index)=fnuniquename$(mat controlname$,index)
94385     next index
94390   fnend 
94395 ! 
94400 ! 
94405   dim addtosubscount
94410 ADDTOSUBSARRAY: ! Adds Subscripts To A Processing Effecient Array
94415   def fnaddtosubsarray(string$*50,mat subs$;___,index)
94420 ! 
94425     let addtosubscount+=1
94430     let index=udim(mat subs$)
94435 ! 
94440     if index=0 or addtosubscount>31 then 
94445       gosub ADDNEWONE
94450     else 
94455       let subs$(index)=subs$(index)&": "&string$ soflow ERRORADDNEWONE
94460     end if 
94465   fnend 
94470 ! 
94475 ERRORADDNEWONE: gosub ADDNEWONE : continue 
94480 ADDNEWONE: ! Add A New Sub Line
94485   let addtosubscount=1
94490   let index+=1
94495   mat subs$(index)
94500   let subs$(index)="*"&string$
94505   return 
94510 ! 
94515 FINDSUBSCRIPT: ! This Function Finds A Subscript And Returns Its Number
94520   def library fnfindsubscript(mat subscripts$,prefix$,string$*40;___,index,subscript,position,equals,colon)
94525     do while index<udim(mat subscripts$)
94530       let index+=1
94535       if (position:=pos(lwrc$(subscripts$(index))," "&lwrc$(trim$(prefix$))&lwrc$(trim$(string$)))) then 
94540         let subscript=-1
94545         let equals=pos(subscripts$(index),"=",position)
94550         let colon=pos(subscripts$(index),":",position)
94555         if ~colon then let colon=len(subscripts$(index))+1
94560         let subscript=val(subscripts$(index)(equals+1:colon-1)) conv ignore
94565       end if 
94570     loop until subscript>0
94575     let fnfindsubscript=subscript
94580   fnend 
94585 ! 
94590   def fnsetforcevisibility(set)
94595     let forcevisibility=fnforcevisibility
94600     let fnchangeforcevisibility(set,1)
94605   fnend 
94610   def fnresetforcevisibility
94615     let fnchangeforcevisibility(forcevisibility,1)
94620   fnend 
94625   def fnchangeforcevisibility(set;forced,___,needed,i)
94630     for i=1 to udim(mat framekeys$)
94635       if trim$(framekeys$(i))<>"" then 
94640         let needed=1
94645       end if 
94650     next i
94655     if needed or forced then 
94660       if set then 
94665         if currentfvstatus><2 then 
94670           execute "config force visibility on"
94675           let currentfvstatus=2
94680         end if 
94685       else 
94690         if currentfvstatus><1 then 
94695           execute "config force visibility off"
94700           let currentfvstatus=1
94705         end if 
94710       end if 
94715     end if 
94720   fnend 
94725   dim currentfvstatus
94730   def fnforcevisibility(;___,filenumber,line$*255,done,fvstat)
94735     if currentfvstatus then 
94740       let fnforcevisibility=currentfvstatus-1
94745     else 
94750       execute "status config >config."&session$
94755       open #(filenumber:=fngetfilenumber): "name=config."&session$,display,input 
94760       do while file(filenumber)=0
94765         linput #filenumber: line$ eof ignore
94770         if pos(uprc$(line$),"FORCE VISIBILITY") then 
94775           if pos(uprc$(line$),"ON") then 
94780             let fvstat=1
94785           end if 
94790           let done=1
94795         end if 
94800       loop while not done
94805       close #filenumber: 
94810       execute "free config."&session$
94815       let fnforcevisibility=fvstat
94820       let currentfvstatus=fvstat+1
94825     end if 
94830   fnend 
94835 ! 
94840 STRINGFUNCTION: ! This Returns True If The Passed In Function Is A String
94845   def fnstringfunction(function$*255;___,start,paren)
94850     if trim$(function$(1:1))="{" then 
94855       if librarylinkage then 
94860         let fnstringfunction=fncheckstringfunction(function$)
94865       else 
94870         let function$=fnreadcustomfunctionstatement$(function$)
94875       end if 
94880     end if 
94885     if trim$(function$(1:1))<>"{" then 
94890       let start=pos(lwrc$(function$),"fn")
94895       let paren=pos(function$,"(",start)
94900       if paren=0 then let paren=len(function$)
94905       let fnstringfunction=pos(function$(start:paren),"$")
94910     end if 
94915   fnend 
94920 ! 
94925 RETURNVALUE: ! Convert A String To A Number According To Special Rules
94930   def fnreturnvalue(value$*1000;___,number)
94935     let number=1 ! If Conversion Failed, Then Its A Non-Null String, Return True
94940     let number=val(value$) conv ignore
94945     let fnreturnvalue=number
94950   fnend 
94955 ! 
94960   def fndisplayloadmessage(;___,window)
94965     let fnchangeforcevisibility(0)
94970     let window=fnopencenteredwindow(3,50,1,trim$(screenio$(si_lockwindow)))
94975     print #window, fields "1,1,CC 50;2,1,CC 50;3,1,CC 50" : "Now Reading Record - Requesting Write Permission","If you see this message for more then a","few seconds the record is probably in use."
94980     let fndisplayloadmessage=window
94985     let fnresetforcevisibility
94990   fnend 
94995 ! 
95000   def fnasklockerror(;lockuser$*800,filename$*255,___,window,function,button2$)
95005     if screenio(si_screeniolocking) then 
95010       let button2$="Continue"
95015     else 
95020       let button2$="Read Only"
95025     end if 
95030     let fnchangeforcevisibility(0)
95035     let window=fnopencenteredwindow(4,60,1,trim$(screenio$(si_lockwindow)),row,col)
95040     print #window, fields "1,1,60/CC "&str$(len(filename$)+25) : "File Sharing Violation - "&filename$
95045     print #window, fields "2,1,CC 60" : "This record is currently in use by"
95050     print #window, fields "3,1,60/CC "&str$(len(lockuser$)+5) : "user "&lockuser$
95055     print #window, fields "4,10,CC 10,/W:W,B40;4,25,CC 10,/W:W,B41;4,40,CC 10,/W:W,B42" : "Retry",button2$,"Cancel"
95060     do 
95065       input #0, fields str$(row+1)&","&str$(col+1)&",C 1,AEX" : kp$
95070       let function=fkey
95075       if function=99 then let function=42
95080       if function=0 then let function=40
95085     loop while function<40 or function>42
95090     let fnasklockerror=function-39
95095     close #window: 
95100     let fnresetforcevisibility
95105   fnend 
95110 ! 
95115   def fnopencenteredwindow(rows,cols;b,spec$*255,&row,&col,___,rs,cs,screenrows,screencols,window)
95120     if b then 
95125       let rs=rows+2
95130       let cs=cols+2
95135       let spec$=spec$&",Border=S"
95140     else 
95145       let rs=rows
95150       let cs=cols
95155     end if 
95160     if env$("guimode")="ON" then 
95165       let fnreadscreensize(screenrows,screencols)
95170     else 
95175       let screenrows=24 : let screencols=80
95180     end if 
95185     let row=int((screenrows-(rs))/2)+2
95190     let col=int((screencols-(cs))/2)+2
95195 ! 
95200     open #(window:=fngetfilenumber): "SROW="&str$(row)&",SCOL="&str$(col)&",ROWS="&str$(rows)&",COLS="&str$(cols)&spec$,display,outin 
95205     let fnopencenteredwindow=window
95210   fnend 
95215 ! 
95220   dim lockuser$(1)*255
95225   def fnretrylockederror(key$*255,filenumber;onread,___,lockuser$*800,choice,_errline,_errnumber,__usr$*80,index)
95230     let _errline=line
95235     let _errnumber=err
95240     if pos("0061_4148",cnvrt$("PIC(####)",err)) then 
95245       let fnreadlockedusers(mat lockuser$)
95250       let lockuser$=""
95255       if udim(mat lockuser$) then 
95260         let lockuser$=lockuser$(1)
95265         for index=2 to udim(mat lockuser$)
95270           let lockuser$=lockuser$&" and "&lockuser$(index)
95275         next index
95280       end if 
95285 ! 
95290 ! .      ! Here is where we link to our LOCKED event
95295       if len(trim$(screenio$(si_lockedfn))) then 
95300         let fnretrylockederror=fnexecute(screenio$(si_lockedfn))
95305       else 
95310         if onread then 
95315           let choice=fnasklockerror(lockuser$,file$(filenumber))
95320           if choice=1 or choice=2 then 
95325             let fnretrylockederror=choice
95330           end if 
95335         else 
95340           let choice=msgbox("The current record is locked in file #"&str$(filenumber)&', "'&file$(filenumber)&'", by user '&trim$(lockuser$)&". Do you want to retry?","Record Locked","Okc","Qst")
95345           if choice=1 then 
95350             let fnretrylockederror=1
95355           end if 
95360         end if 
95365       end if 
95370     end if 
95375   fnend 
95380   def fnretrytimeouterror(;___,window,rows,cols,row,col,count)
95385     if len(screenio$(si_waitfn)) then 
95390       let fnresetforcevisibility
95395       let fnretrytimeouterror=fnexecute(screenio$(si_waitfn))
95400     else 
95405       let fnchangeforcevisibility(0)
95410       let window=fnopencenteredwindow(3,40,1,trim$(screenio$(si_lockwindow)))
95415       print #window, fields "1,1,CC 40" : "Your system has been idle for some time."
95420       print #window, fields "2,1,CC 40" : "Press any key to continue or you will"
95425       print #window, fields "3,1,CC 40" : "be logged out automatically."
95430       do while count<300 ! 30 Seconds
95435         let count+=1
95440         let sleep(.1)
95445         if len(kstat$) then 
95450           let fnretrytimeouterror=1
95455           exit do 
95460         end if 
95465       loop 
95470       close #window: 
95475     end if 
95480     let fnchangeforcevisibility(1)
95485   fnend 
95490 ! 
99000 ! #Autonumber# 99000,10
99010 OPEN: ! ***** Function To Call Library Openfile And Proc Subs
99020   def fnopen(filename$*255, mat f$, mat f, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,supress_prompt,ignore_errors,___,index)
99030     dim _fileiosubs$(1)*800, _loadedsubs$(1)*80
99040     let fnopen=fnopenfile(filename$, mat f$, mat f, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$, supress_prompt,ignore_errors,program$)
99050     if srch(_loadedsubs$,uprc$(filename$))<=0 then : mat _loadedsubs$(udim(_loadedsubs$)+1) : let _loadedsubs$(udim(_loadedsubs$))=uprc$(filename$) : for index=1 to udim(mat _fileiosubs$) : execute (_fileiosubs$(index)) : next index
99060   fnend 
99070 ! 
99080 ! 
99500 ! #Autonumber# 99500,10
99510 IGNORE: continue 
99520 ! 
99530 RETRYFIVETIMES: ! 
99540   let retryfivetimes+=1
99550   if retryfivetimes<10 then retry 
99560   continue 
99570 ! 
99580   dim retryaction
99590   dim trytocontinue
99600 ERRORFILEISLOCKED: ! Ask The User To Retry Or Abort
99610   let retryaction=fnretrylockederror(key$,fdatafile,1)
99620   if retryaction=1 then 
99630     retry 
99640   else if retryaction=2 then 
99650     let trytocontinue=1
99660   else 
99670     let exitmode=1
99680     continue 
99690   end if 
99700 ! 
99710 ERRORFILELOCKEDONEXIT: ! Ask The User To Retry Or Abort
99720   if fnretrylockederror(key$,fdatafile) then 
99730     retry 
99740   else 
99750     let exitmode=0 ! Cancel the exit
99760     continue 
99770   end if 
99780 ! 
99790 TIMEOUTERROR: ! A Timeout Has Occured
99800   if fnretrytimeouterror then 
99810     retry 
99820   else 
99830     let exitmode=1
99840     continue 
99850   end if 
