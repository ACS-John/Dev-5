 ! run.br - ScreenIO Run Screen Library        #AutoNumber# 1000,10
 !
 ! Created 1/1/2009 by Gabriel Bakker
 !
 !
 !  This library runs screens from a function to preserve library variables
 !  needed by calling screen.
 !
 !  Also, this library will make a tab by combining several screenIO screens
 !

    def library fnRun(Screen$*255;key$*255,ParentKey$*255,DisplayOnly,ParentWindow,DontRedoListviews) ! Run a screen through a function
       let fnEstablishLibraryLinkage
       let fnCallScreen$(Screen$,key$,ParentKey$,DisplayOnly,ParentWindow,DontRedoListviews)
    fnend

    def library fnRunTab(&ExitMode,TabExitMode,Screen$*255;key$*255,ParentKey$*255,DisplayOnly,ParentWindow,DontRedoListviews) ! Run a screen on a tab
       let fnEstablishLibraryLinkage
       if len(trim$(fnCallScreen$(Screen$,key$,ParentKey$,DisplayOnly,ParentWindow,DontRedoListviews))) and fkey<>92 then
          let fnRunTab=1
       end if
       if fkey=92 then let ExitMode=TabExitMode
    fnend

    def library fnTabs(mat Screen$,mat Caption$;Key$*255,ParentKey$*255,FileLay$,Parentwindow,StartScreen,BlockESC,ExitOnCancel,Predraw,MsgScreen$,MsgRow,MsgCol,Debug,RecordNum,Path$*255,AskSaveTogether,mat IgnoreStrings,mat IgnoreNumbers)
       let fnTabs=fnReturnValue(fn_Tabs$(mat Screen$,mat Caption$,0,0,Predraw,MsgScreen$,MsgRow,MsgCol,StartScreen,BlockESC,ExitOnCancel,Key$,ParentKey$,Debug,ParentWindow,FileLay$,RecordNum,Path$,AskSaveTogether,mat IgnoreStrings,mat IgnoreNumbers))
    fnend

    def library fnTabs$(mat Screen$,mat Caption$;Key$*255,ParentKey$*255,FileLay$,Parentwindow,StartScreen,BlockESC,ExitOnCancel,Predraw,MsgScreen$,MsgRow,MsgCol,Debug,RecordNum,Path$*255,AskSaveTogether,mat IgnoreStrings,mat IgnoreNumbers)
       let fnTabs$=fn_Tabs$(mat Screen$,mat Caption$,0,0,Predraw,MsgScreen$,MsgRow,MsgCol,StartScreen,BlockESC,ExitOnCancel,Key$,ParentKey$,Debug,ParentWindow,FileLay$,RecordNum,Path$,AskSaveTogether,mat IgnoreStrings,mat IgnoreNumbers)
    fnend

    def library fnRunTabs(mat Screen$,mat Caption$,Rows,Cols;InsideScreen,Predraw,MsgScreen$,MsgRow,MsgCol,StartScreen,BlockESC,ExitOnCancel,Key$*255,ParentKey$*255,Debug,Parentwindow,FileLay$,RecordNum,Path$*255,AskSaveTogether,mat IgnoreStrings,mat IgnoreNumbers,DisplayOnly)
       let fnRunTabs=fnReturnValue(fn_Tabs$(mat Screen$,mat Caption$,Rows,Cols,Predraw,MsgScreen$,MsgRow,MsgCol,StartScreen,BlockESC,ExitOnCancel,Key$,ParentKey$,Debug,ParentWindow,FileLay$,RecordNum,Path$,AskSaveTogether,mat IgnoreStrings,mat IgnoreNumbers,DisplayOnly))
    fnend

    def library fnRunTabs$(mat Screen$,mat Caption$,Rows,Cols;InsideScreen,Predraw,MsgScreen$,MsgRow,MsgCol,StartScreen,BlockESC,ExitOnCancel,Key$*255,ParentKey$*255,Debug,ParentWindow,FileLay$,RecordNum,Path$*255,AskSaveTogether,mat IgnoreStrings,mat IgnoreNumbers,DisplayOnly)
       let fnRunTabs$=fn_Tabs$(mat Screen$,mat Caption$,Rows,Cols,Predraw,MsgScreen$,MsgRow,MsgCol,StartScreen,BlockESC,ExitOnCancel,Key$,ParentKey$,Debug,ParentWindow,FileLay$,RecordNum,Path$,AskSaveTogether,mat IgnoreStrings,mat IgnoreNumbers,DisplayOnly)
    fnend

    dim F$(1)*1024, F(1)
    dim Read_F$(1)*1024,Read_F(1)
    dim ScreenIO$(1)*1024,ScreenIO(1)
    dim Attribute$(1)*255
    dim Form$(1)*255

    def fn_Tabs$(mat Screen$,mat Caption$;Rows,Cols,Predraw,MsgScreen$,MsgRow,MsgCol,StartScreen,BlockESC,ExitOnCancel,&Key$,&ParentKey$,Debug,ParentWindow,FileLay$,RecordVal,&Path$,&AskSaveTogether,mat IgnoreStrings,mat IgnoreNumbers,DisplayOnly,___,Index,CurrentScreen,dummy$,ReturnValue$*255,Return$*255,StartTime,ParentWindow$,FileNumber,Skip,ScreenIO,BigRow,BigCol,NAttr$*255,Choice,SaveKey$*127,SaveRecord,HiddenWindow,HiddenInput$,CancelledExit)
       let fnEstablishLibraryLinkage

       let fnPushData(1)

       dim Inputwindows(1)
       dim PredrawWindows(1)
       dim PredrawMessage(1)
       mat InputWindows(udim(mat screen$))
       mat PredrawWindows(udim(mat screen$))
       mat PredrawMessage(udim(mat screen$))

       if ParentWindow then let ParentWindow$=",parent="&Str$(ParentWindow)

       if DisplayOnly then
          open #(HiddenWindow:=fnGetFileNumber): "srow=3,scol=3,rows=1,cols=1"&ParentWindow$,display,outin
       end if

       ! Preinspect screen record the attributes and the color settings, compile into Attributes list.
       let ScreenIO=fnOpen("screenio",mat ScreenIO$,mat ScreenIO,mat Form$,1)
       mat Attribute$(udim(mat Screen$))=("")
       for Index=1 to udim(mat Screen$)
          mat ScreenIO$=("") : mat ScreenIO=(0)
          let ScreenIO$(si_screencode)=uprc$(Screen$(Index))
          read #ScreenIO, using form$(ScreenIO), key=fnBuildKey$("screenio",mat ScreenIO$,mat Screenio) : mat ScreenIO$, mat ScreenIO nokey Ignore
          if file(ScreenIO)=0 then
             ! Also find largest rows and columns and use that for Rows and Cols if they aren't given
             if ScreenIO(si_hsize)>BigCol then bigCol=ScreenIO(si_hsize)
             if ScreenIO(si_vsize)>BigRow then bigRow=ScreenIO(si_vsize)

             if pos(ScreenIO$(si_attributes),"N=") then
                let NAttr$=screenIO$(si_attributes)(pos(ScreenIO$(si_attributes),"N="):9999)
                if pos(NAttr$,",") then
                   let NAttr$=NAttr$(1:pos(NAttr$,",")-1)
                end if
                attribute$(Index)=","&NAttr$
             else if len(trim$(ScreenIO$(si_bgcolor))) then
                if len(ScreenIO$(si_bgcolor))=6 then
                   let ScreenIO$(si_bgcolor)(1:0)="#"
                end if
                attribute$(Index)=",N=/W:"&screenIO$(si_bgcolor)
             end if
          end if
       next Index
       close #ScreenIO:

       if Rows=0 then let Rows=BigRow
       if Cols=0 then cols=BigCol

       for Index=1 to udim(mat Screen$)
          open #(InputWindows(Index):=fnGetFileNumber): "srow=2,scol=2,rows="&str$(Rows)&",cols="&str$(Cols)&Attribute$(Index)&",tab="&Caption$(Index)&ParentWindow$, display, outin
       next Index

       if StartScreen then
          currentScreen=StartScreen
          input #InputWindows(CurrentScreen), fields "2,2,C 1,,NOWAIT" : dummy$
          pr #InputWindows(CurrentScreen), fields "2,2,C 1" : ""
       else
          currentscreen=1
       end if

       if Predraw or DisplayOnly then
          let StartTime=timer
          let PredrawWindows(Currentscreen)=fnfm(Screen$(Currentscreen),Key$,1,1,ParentKey$,InputWindows(Currentscreen),1,1,RecordVal)
          if Debug then pr Screen$(Currentscreen)&": "&str$(timer-StartTime)&" second(s)."

          if MsgScreen$<>"" then
             let PredrawMessage(Currentscreen)=fnfm(MsgScreen$,Key$,MsgRow,MsgCol,ParentKey$,PredrawWindows(Currentscreen),1,1,RecordVal)
          end if
          let scr_freeze
       else
          if MsgScreen$<>"" then
             let PredrawMessage(Currentscreen)=fnfm(MsgScreen$,Key$,MsgRow,MsgCol,ParentKey$,InputWindows(Currentscreen),1,1,RecordVal)
             let scr_freeze
          end if
       end if

       for Index=1 to udim(mat Screen$)
          if Index<>CurrentScreen then ! Skip the one we already did
             if Predraw or DisplayOnly then
                let StartTime=timer
                let PredrawWindows(Index)=fnfm(Screen$(Index),Key$,1,1,ParentKey$,InputWindows(Index),1,1,RecordVal)
                if Debug then pr Screen$(Index)&": "&str$(timer-StartTime)&" second(s)."
                if MsgScreen$<>"" then
                   let PredrawMessage(Index)=fnfm(MsgScreen$,Key$,MsgRow,MsgCol,ParentKey$,PreDrawWindows(Index),1,1,RecordVal)
                end if
                let scr_freeze
             else
                if MsgScreen$<>"" then
                   let PredrawMessage(Index)=fnfm(MsgScreen$,Key$,MsgRow,MsgCol,ParentKey$,InputWindows(Index),1,1,RecordVal)
                   let scr_freeze
                end if
             end if
          end if
       next Index

       if DisplayOnly then
          ! Display Only Processing
          do
             input #HiddenWindow, fields "1,1,C 1" : HiddenInput$
          loop until (fkey=99 and ~BlockESC) or fkey=93 or (fkey<>92 and ExitOnCancel)
       else

          if len(FileLay$) then
             mat f=(0)
             mat f$=("")
             let FileNumber=fnOpen(FileLay$,mat F$,mat F,mat Form$)
             if len(trim$(Key$)) then
                let WarnWindow=fnDisplayLoadMessage
                read #FileNumber, using form$(Filenumber), key=Key$ : mat F$, mat F nokey Ignore locked ErrorFileLocked
                close #WarnWindow:
                if ~Skip and File(FileNumber) then ! key not found
                   let msgbox("The Key "&trim$(Key$)&" could not be found.")
                   let Skip=1
                else
                   let SaveKey$=Key$
                end if
             else if RecordVal then
                let WarnWindow=fnDisplayLoadMessage
                read #FileNumber, using form$(Filenumber), rec=RecordVal : mat F$, mat F nokey Ignore locked ErrorFileLocked
                close #WarnWindow:
                if ~Skip and File(FileNumber) then ! key not found
                   let msgbox("The Record "&str$(RecordVal)&" could not be found.")
                   let Skip=1
                else
                   let SaveRecord=RecordVal
                end if
             end if
             if AskSaveTogether then
                ! Store what we started with so we can see if they've changed
                !  it.
                mat Read_F$(udim(mat F$))=F$
                mat Read_F(udim(mat F))=F
             end if
          end if

          if ~Skip then
             do
                if len(FileLay$) then
                   ! If filelay is given, then they wanted to read the file first. Use mat MyF$ and mat MyF
                   !  no key or record value is given because ScreenIO won't be reading the file, instead it'll be using MyF
                   let ReturnValue$=fnfm$(Screen$(CurrentScreen),"",1,1,ParentKey$,InputWindows(CurrentScreen),0,1,0,Dummy$,1,mat F$,mat F,"",0,AskSaveTogether)
                else
                   ! If filelay not given, just call the screens and let them handle it how they may.
                   let ReturnValue$=fnfm$(Screen$(CurrentScreen),Key$,1,1,ParentKey$,InputWindows(CurrentScreen),0,1,RecordVal)
                end if

                if CurrentScreen=1 and fkey<>92 then
                   let fn_Tabs$=ReturnValue$
                end if
                let scr_freeze

                if Fkey=92 then ! If A Tab Is Clicked Then Jump To That Tab
                   if Srch(Mat Inputwindows,Curtab(InputWindows(1)))>0 then
                      currentscreen=Srch(Mat Inputwindows,Curtab(InputWindows(1)))
                   end if
                end if
                cancelledExit=0
                if (fkey=99 and ~BlockESC) or fkey=93 or (fkey<>92 and ExitOnCancel) then          ! We're about to exit
                   if len(returnvalue$) and len(FileLay$) then                                     ! and they didn't cancel, and they chose to write data at the end
                      if AskSaveTogether then                                                      ! They requested we ask here instead of in the screens
                         if ~Fnsamea(Mat Read_F,Mat F,mat IgnoreNumbers) Or ~Fnsameas(Mat Read_F$,Mat F$,mat IgnoreStrings) then       ! Something has changed
                            ! Ask if they want to save or not.
                            choice=Msgbox("The data has changed. Do you want to accept the changes?","Save?","ynC","QST")
                            if Choice=2 then  ! They said YES
                               ! Leave things alone
                            else if Choice=3 then ! They said NO
                               let ReturnValue$="" ! As if they said NO, don't save
                            else ! They said CANCEL
                               let fkey(0) ! Cancel the exit
                               cancelledExit=1
                            end if
                         else                   ! if they haven't changed
                            let ReturnValue$="" ! As if they said NO, don't save
                         end if
                      end if
                   end if
                end if
             loop Until (~CancelledExit) and ((fkey=99 and ~BlockESC) or fkey=93 or (fkey<>92 and ExitOnCancel))

             if len(FileLay$) then
                if len(ReturnValue$) then
                   if len(trim$(SaveKey$)) then
                      rewrite #FileNumber, using form$(FileNumber), key=SaveKey$ : mat F$, mat F
                   else if RecordVal then
                      rewrite #FileNumber, using form$(FileNumber), rec=SaveRecord : mat F$, mat F
                   else
                      write #FileNumber, using form$(FileNumber) : mat F$, mat F
                   end if
                end if
             end if
          end if
          if len(FileLay$) then
             let fnCloseFile(FileNumber,FileLay$,"",1)
          end if
       end if
       
       if DisplayOnly then
          close #HiddenWindow:
       end if
 !
       for Index=1 to Udim(Mat Inputwindows)
          if MsgScreen$<>"" then close #PredrawMessage(Index):
          if Predraw then close #PredrawWindows(Index):
          close #Inputwindows(Index):
       next Index

       let fnPopData
    fnend

    def library Fnrunedit(Screenname$,Ckey$*255)
       library "screenio" : Fnfm
       if Len(Trim$(Ckey$)) then let Fnfm(Screenname$,Ckey$)
    fnend
 !
    def library Fnruneditrec(Screenname$,Crec)
       library "screenio" : Fnfm
       if Crec then let Fnfm(Screenname$,"",0,0,"",0,0,0,Crec)
    fnend
    
 ! #AutoNumber# 80000,10

 STACK: ! Functions To Manage The Stack
    dim Stack$(1)*255
    dim Longstack$(1)*2047
    dim Stack(1)
 !
    def fnPushData(;Wipe)
       let Fnpush(Mat PredrawMessage,Wipe)
       let Fnpush(Mat PredrawWindows,Wipe)
       let fnPush(mat Inputwindows,Wipe)
    fnend
    def fnPopData
       let fnPop(Mat InputWindows)
       let Fnpop(Mat PredrawWindows)
       let Fnpop(Mat PredrawMessage)
    fnend

    def Fnpush$(Mat Array$,Wipe)
       let Fnpush$=Fnpusharray$(Mat Array$,Mat Stack$,Mat Longstack$)
       if Wipe then
          mat Array$(0)
       end if
    fnend
    def Fnpop$(Mat Array$)
       let Fnpop$=Fnpoparray$(Mat Array$,Mat Stack$,Mat Longstack$)
    fnend
    def Fnpush(Mat Array;Wipe)
       let Fnpush=Fnpusharray(Mat Array,Mat Stack)
       if Wipe then
          mat Array(0)
       end if
    fnend
    def Fnpop(Mat Array)
       let Fnpop=Fnpoparray(Mat Array,Mat Stack)
    fnend
    dim PushValue$(1)*2047,PushValue(1)
    def fnPushValue$(&Var$;Altstack,Wipe)
       mat PushValue$(1)
       let PushValue$(1)=Var$
       let fnPush$(mat PushValue$,AltStack,Wipe)
       if Wipe then let Var$=""
    fnend
    def fnPopValue$(&Var$;Altstack)
       let fnPop$(mat PushValue$,AltStack)
       let Var$=PushValue$(1)
    fnend
    def fnPushValue(&Var;Altstack,Wipe)
       mat PushValue(1)
       let PushValue(1)=Var
       let fnPush(mat PushValue,Altstack,Wipe)
       if Wipe then let Var=0
    fnend
    def fnPopValue(&Var;Altstack)
       let fnPop(mat PushValue,Altstack)
       let Var=PushValue(1)
    fnend

    dim PushWorkArray$(1)*4000
    def Fnpusharray$(Mat Array$,Mat Stack$,Mat Longstack$;___,Startindex,Endindex,Size,Index)
       mat PushWorkArray$(udim(mat Array$))=Array$
       for Index=1 to Udim(Mat PushWorkArray$)
          if Len(PushWorkArray$(Index))>255 then
             mat Longstack$(Udim(Mat Longstack$)+1)
             let Longstack$(Udim(Mat Longstack$))=PushWorkArray$(Index)
             let PushWorkArray$(Index)="[[[loNgsTaCk]]]"
          end if
       next Index
 !
       let Startindex=Udim(Mat Stack$)+1
       let Size=Udim(Mat PushWorkArray$)
       let Endindex=Startindex+Size-1
       mat Stack$(Endindex+1)
       if Size then
          mat Stack$(Startindex:Endindex)=PushWorkArray$
       end if
       let Stack$(Endindex+1)=Str$(Size)
    fnend
    def Fnpoparray$(Mat Array$,Mat Stack$,Mat Longstack$;___,Startindex,Endindex,Size,Index)
       let Endindex=Udim(Mat Stack$)-1
       let Size=Val(Stack$(Endindex+1))
       let Startindex=Endindex-Size+1
       mat Array$(Size)
       if Size then
          mat Array$=Stack$(Startindex:Endindex)
       end if
       mat Stack$(Startindex-1)
 !
       for Index=Udim(Mat Array$) to 1 step -1
          if Array$(Index)="[[[loNgsTaCk]]]" then
             array$(Index)=Longstack$(Udim(Mat Longstack$))
             mat Longstack$(Udim(Mat Longstack$)-1)
          end if
       next Index
    fnend
    def Fnpusharray(Mat Array,Mat Stack;___,Startindex,Endindex,Size)
       let Startindex=Udim(Mat Stack)+1
       let Size=Udim(Mat Array)
       let Endindex=Startindex+Size-1
       mat Stack(Endindex+1)
       if Size then
          mat Stack(Startindex:Endindex)=Array
       end if
       let Stack(Endindex+1)=Size
    fnend
    def Fnpoparray(Mat Array,Mat Stack;___,Startindex,Endindex,Size)
       let Endindex=Udim(Mat Stack)-1
       let Size=Stack(Endindex+1)
       let Startindex=Endindex-Size+1
       mat Array(Size)
       if Size then
          mat Array=Stack(Startindex:Endindex)
       end if
       mat Stack(Startindex-1)
    fnend

 !  #AutoNumber# 90000,10
    def fnDisplayLoadMessage(;___,Window)
       open #(Window:=Fngetfilenumber): "SROW=12,SCOL=15,ROWS=3,COLS=50,Border=S",display,outin
       pr #Window, fields "1,1,CC 50" : "Now Reading Record - Requesting Write Permission"
       pr #Window, fields "2,1,CC 50" : "If you see this message for more then a"
       pr #Window, fields "3,1,CC 50" : "few seconds the record is probably in use."
       let fnDisplayLoadMessage=Window
    fnend
 !
    def Fnretrylockederror(Key$*255,Filenumber;___,Lockfile,LockUser$*80,Choice,_ErrLine,_ErrNumber)
       let _ErrLine=Line
       let _ErrNumber=Err
       if Pos("0061_4148",Cnvrt$("PIC(####)",Err)) then
          execute "Status locks >ERTMP[SESSION]"
          open #(Lockfile:=Fngetfilenumber): "NAME=ERTMP[SESSION]",display,input
          do While LockUser$(1:1)<>"-"
             linput #Lockfile: LockUser$
          loop
          linput #Lockfile: LockUser$
          if Ltrm$(LockUser$(11:14))=Ltrm$(Wsid$) then
             linput #Lockfile: LockUser$ error Ignore
          end if
          let LockUser$=LockUser$(26:36)
          close #Lockfile:
          execute "*free ERTMP[SESSION]"
 !
          choice=Msgbox("This record is locked in file #"&Str$(Filenumber)&', "'&File$(Filenumber)&'", by user '&Trim$(LockUser$)&". Do you want to retry?","Record Locked","Okc","Qst")
          if Choice=1 then
             let Fnretrylockederror=1
          end if
       end if
    fnend

 ErrorFileLocked: ! Ask The User To Retry Or Abort
    if Fnretrylockederror(Key$,FileNumber) then
       retry
    else
       let Skip=1 ! Cancel the edit
       continue
    end if
 !
 ! #Autonumber# 99000,10
    def fnEstablishLibraryLinkage
       if ~LinkageEstablished then
          let LinkageEstablished=1
          library "fileio" : fnGetFileNumber, fnOpenFile, fnCloseFile, fnBuildKey$
          library "screenio" : fnFm$, fnfm, fnCallScreen$
       end if
    fnend
 !
    def Fnreturnvalue(Value$*1000;___,Number)
       let Number=1 ! If Conversion Failed, Then Its A Non-Null String, Return True
       let Number=Val(Value$) conv IGNORE
       let Fnreturnvalue=Number
    fnend
 !
 !  ***** Compares Two String Arrays Returning True If Same
    def Fnsameas(Mat A$,Mat B$;mat IgnoreThese,___,Failed,Index)
       if Udim(Mat A$)=Udim(Mat B$) then
          for Index=1 to Udim(Mat A$)
             if srch(mat IgnoreThese,Index)<=0 then
                if trim$(trim$(Trim$(A$(Index)),chr$(0)))<>trim$(trim$(Trim$(B$(Index)),chr$(0))) then let Failed=1
             end if
          next Index
          let Fnsameas=(~(Failed))
       end if
    fnend
 !
 !  ***** Compares  Two Number Arrays Returning True If Same
    def Fnsamea(Mat A,Mat B;mat IgnoreThese,___,Failed,Index)
       if Udim(Mat A)=Udim(Mat B) then
          for Index=1 to Udim(Mat A)
             if srch(mat IgnoreThese,Index)<=0 then
                if A(Index)<>B(Index) then let Failed=1
             end if
          next Index
          let Fnsamea=(~(Failed))
       end if
    fnend

 OPEN: ! ***** Function To Call Library Openfile And Proc Subs
       def Fnopen(Filename$*255, Mat F$, Mat F, Mat Form$; Inputonly, Keynum, Dont_Sort_Subs, Path$*255, Mat Descr$, Mat Field_Widths,Supress_Prompt,Ignore_Errors,___,Index)
          dim _FileIOSubs$(1)*800, _Loadedsubs$(1)*32
          let Fnopen=Fnopenfile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$)
          if Srch(_Loadedsubs$,Uprc$(Filename$))<=0 then : mat _Loadedsubs$(Udim(_Loadedsubs$)+1) : let _Loadedsubs$(Udim(_Loadedsubs$))=Uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index
       fnend
 !
 ! #Autonumber# 99980,10
 IGNORE: continue