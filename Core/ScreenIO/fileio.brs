 ! Fileio - Copyright February 2006 By Gabriel Bakker #Autonumber# 10,10
 !    Distributed Open Source As A Christmas Gift To the BR Community
 ! This Library Houses All File Maintenance Operations
 !
 ! Created: 03/28/06
 ! Modified: 06/14/16
 !
 ! Version 79
 !
 !
 ! #Autonumber# 500,1
 ! If You Run This Library As A Program, It Will Function As A Data
 ! Crawler That Will Allow You To View The Raw Data In Your Datafiles
 !
 ! To Update A Datafile, Just Simply Increase The Version Number
 ! Then Make Any Change You Like - Add Fields, Remove Fields, Rearrange
 ! Fields, Change Keys...
 !
 ! Then Just Run Your Software Normally - Upon First Time, Version
 ! Discrepancy Is Detected, And File Is Updated Dynamically.
 !
 ! Note: Do Not Change Your Subscript Names Between Versions.
 ! They Can Be Rearranged, And You Can Insert New Data, But The Fileio
 ! Library Uses The Subscript Names To Ensure The Proper Data Is Placed
 ! In The Correct Spot In The New Version Of The Data File During The
 ! Update.
 !
 ! Global Settings Such As The Default File Layout Path May Be Set
 ! By creating a simple procfile called fileio.ini, a procfile that
 ! resides in the current path and setting the appropriate values.
 !
    goto DIMS
 !
 ! #Autonumber# 1000,5
 SETTINGS: ! ***** Set Global Defaults For The Library Here
    def FnDefaultSettings(&Defaultfilelayoutpath$;&Promptonfilecreate,&PromptOnUpdate,&Createlogfile,&EnforceDupkeys,&StartFileNumber,&CheckIndex,&CompressColumns,&MaxColWidth,&NewLine$,&LogLibrary$,&LogLayout$,&AnimateDatacrawler,&TemplatePath$,&IgnoreLayouts$,&CloseFileSimple,&AuditPath$,&ScreenIOPath$,&FileLayoutExtension$,&FilterBoxFileList,&FilterBoxData)
       enforceDupkeys=1  ! Enforce that key number 1 is unique key
       defaultfilelayoutpath$="filelay\" ! Path To Your File Layouts
       fileLayoutExtension$="" ! Extension for your File Layouts
       promptonfilecreate=1 ! Ask User Before Creating New Files
       promptOnUpdate=2 ! 0 - Suppress Prompt, 1- Show Prompt, 2- Use PromptOnFileCreate
       createlogfile=0  ! 1 to turn on Logging. Caution, the log file can get big very quickly.
       startFileNumber=1 ! Set above 255 to avoid conflicts with legacy programs.
       checkIndex=0 ! Automatically Update Indexes (Use for partial fileio implementations, disable for full fileio installations.)
       compressColumns=0 ! Shrink or Expand Columns in Data Crawler by Default
       maxColWidth=20 ! Max Default Column Width in Data Crawler
       newLine$=Chr$(13)&Chr$(10) ! Default CRLF
       logLibrary$="" ! Log Library to use, defaults to None (use FileIO's internal log functionality)
       logLayout$="" ! Log file layout, defaults to Internal File (for use with FileIO's internal log functionality)
       animateDatacrawler=1 ! Use ScreenIO Animation for Datacrawler (if available, will not cause error if not available)
       templatePath$="filelay\template\" ! Default Template Path
       ignoreLayouts$="" ! List any Ignore Layouts here.
       closeFileSimple=0 ! Use simple comparison for fnCloseFile
       auditPath$="auditbr\" ! Backup Path to use for Auditing Files
       screenIOPath$="screenio.br" ! Default search location for ScreenIO Library (for fnAnimate in Data Crawler.)
       filterBoxFileList=0 ! Use a Filter Box for the Layout Listview (1=Filter Box, 0=Search Box)
       filterBoxData=1 ! Use a Filter Box for the Data Crawler (1=Filter Box, 0=Search Box)
    fnend

    def fnReadSettings(mat SSettingNames$,mat SSettings$,mat NSettingNames$,mat NSettings$,&ReadSettings;___,Defaultfilelayoutpath$*255,NewLine$*255,LogLibrary$*255,Promptonfilecreate,PromptOnUpdate,Createlogfile,EnforceDupkeys,StartFileNumber,CheckIndex,CompressColumns,MaxColWidth,LogLibrary$*255,LogLayout$,AnimateDatacrawler,TemplatePath$*255,IgnoreLayouts$*255,CloseFileSimple,Star$,AuditPath$*255,ScreenIOPath$*255,FileLayoutExtension$,FilterBoxFileList,FilterBoxData)
       star$=fnNeedStar$

       ! Read Defaults
       fnDefaultSettings(Defaultfilelayoutpath$,Promptonfilecreate,PromptOnUpdate,Createlogfile,EnforceDupkeys,StartFileNumber,CheckIndex,CompressColumns,MaxColWidth,NewLine$,LogLibrary$,LogLayout$,AnimateDatacrawler,TemplatePath$,IgnoreLayouts$,CloseFileSimple,AuditPath$,ScreenIOPath$,FileLayoutExtension$,FilterBoxFileList,FilterBoxData)

       ! Read Custom Settings
       if exists("fileio.ini") then
          execute Star$&"subproc fileio.ini"
       else if exists("fileio\fileio.ini") then
          execute Star$&"subproc fileio\fileio.ini"
       else if exists("\fileio.ini") then
          execute Star$&"subproc \fileio.ini"
       end if

       ! Detect fnAnimate routines, enable/disable Loading Animation
       if AnimateDatacrawler and exists(ScreenIOPath$) then
          library ScreenIOPath$ : fnAnimate, fnPrepareAnimation, fnCloseAnimation
       else
          animateDatacrawler=0
       end if
       
       ! Calculate Special Settings
       if LogLayout$="" and exists(DefaultFileLayoutPath$&"logfile"&FileLayoutExtension$) then logLayout$="logfile"
       if LogLayout$<>"" then ignoreLayouts$=IgnoreLayouts$&","&LogLayout$
       ignoreLayouts$=lwrc$(IgnoreLayouts$)
       if PromptOnUpdate=2 then promptOnUpdate=Promptonfilecreate

       ! Load Settings Engine
       sSettingNames$(1)="layoutpath"
       sSettingNames$(2)="newline"
       sSettingNames$(3)="loglibrary"
       sSettingNames$(4)="loglayout"
       sSettingNames$(5)="templatepath"
       sSettingNames$(6)="ignorelayouts"
       sSettingNames$(7)="screeniopath"
       sSettingNames$(8)="auditpath"
       ssettingnames$(9)="layoutextension"

       nSettingNames$(1)="promptonfilecreate"
       nSettingNames$(2)="createlogfile"
       nSettingNames$(3)="enforcedupkeys"
       nSettingNames$(4)="startfilenumber"
       nSettingNames$(5)="checkindex"
       nSettingNames$(6)="compresscolumns"
       nSettingNames$(7)="maxcolwidth"
       nSettingNames$(8)="animatedatacrawler"
       nsettingnames$(9)="closefilesimple"
       nSettingNames$(10)="filterboxfilelist"
       nSettingNames$(11)="filterboxdata"
       nSettingNames$(12)="promptonupdate"

       sSettings$(1)=Defaultfilelayoutpath$
       sSettings$(2)=NewLine$
       sSettings$(3)=LogLibrary$
       sSettings$(4)=LogLayout$
       sSettings$(5)=TemplatePath$
       sSettings$(6)=IgnoreLayouts$
       sSettings$(7)=ScreenIOPath$
       sSettings$(8)=AuditPath$
       ssettings$(9)=FileLayoutExtension$

       nSettings(1)=Promptonfilecreate
       nSettings(2)=Createlogfile
       nSettings(3)=EnforceDupkeys
       nSettings(4)=StartFileNumber
       nSettings(5)=CheckIndex
       nSettings(6)=CompressColumns
       nSettings(7)=MaxColWidth
       nSettings(8)=AnimateDatacrawler
       nSettings(9)=CloseFileSimple
       nSettings(10)=filterboxfilelist
       nSettings(11)=filterboxData
       nSettings(12)=PromptOnUpdate

       readSettings=1
    fnend

    dim ReadSettings
    dim SSettingNames$(9),SSettings$(9)*255
    dim NSettingNames$(12),NSettings(12)

    def fnSettings$*255(SettingName$;___,SettingSub)
       if ~ReadSettings then let fnReadSettings(mat SSettingNames$,mat SSettings$,mat NSettingNames$,mat NSettings$,ReadSettings)
       settingSub=srch(mat SSettingNames$,lwrc$(SettingName$))
       if SettingSub>0 then let fnSettings$=SSettings$(SettingSub)
    fnend
    def fnSettings(SettingName$;___,SettingSub)
       if ~ReadSettings then let fnReadSettings(mat SSettingNames$,mat SSettings$,mat NSettingNames$,mat NSettings$,ReadSettings)
       settingSub=srch(mat NSettingNames$,lwrc$(SettingName$))
       if SettingSub>0 then let fnSettings=NSettings(SettingSub)
    fnend
    
    dim StarsChecked
    def fnNeedStar$
       if StarsChecked then
          if StarsChecked=-1 then let fnNeedStar$="*"
       else
          if Env$("guimode")="ON" then
             starsChecked=1
          else
             starsChecked=-1
             fnNeedStar$="*"
          end if
       end if
    fnend

 ! #Autonumber# 2000,10
 DIMS: ! Dimension Statements For Data Crawler
    dim Lvdir_Headings$(1)*30,Lvdir_Widths(1),Lvdir_Fieldforms$(1),lvdir_Listing$(1)*128
    dim Dc_Data$(1)*1024,Dc_Data(1),Dc_Forms$(1)*2000,Dc_Dfdescr$(1)*80,Dc_Dflabel$(1)*80,Dc_Dfshow(1)
    dim Csv_Data$(1)*4000,Csv_Data(1),Csv_Forms$(1)*2000
    dim Dc_Widths(1),Fieldforms$(1),Dc_Outputrow$(1)*1024,Dc_Records(1)
    dim dc_Savedata$(1)*1024, dc_Savesubs(1)
    dim dc_Deleterow(1),dc_Colorstart(1),dc_Colorend(1),dc_Color$(1)
    dim dc_Changeddata$(1)*1024, dc_Changedsubs(1)
    dim _Dummy$*255,_Dummy,_Logging
    dim IncludeCols$(1), UIOptions$(13), SelectedRecords(1),Records(1)
    dim Search$*80
    
    dim FilterBox$*64
 !
    if Trim$(Env$("guimode"))="" then
       pr "DataCrawler Mode only works with New Gui version of BR"
       stop
    end if
    turnguibackoff=(Env$("guimode")=="OFF")
    if Turnguibackoff then execute "config gui on"
 !
    lvdir_Headings$(1)="File Layouts" !:
    lvdir_Widths(1)=38 !:
    lvdir_Fieldforms$(1)="CC 30"
    
    uIOptions$(1)="quitbutton"
    uIOptions$(2)="exportbutton"
    uIOptions$(3)="importbutton"
    uIOptions$(4)="columnsbutton"
    uIOptions$(5)="keybutton"
    uIOptions$(6)="deletebutton"
    uIOptions$(7)="savebutton"
    uIOptions$(8)="addbutton"
    uIOptions$(9)="search"
    uIOptions$(10)="border"
    uIOptions$(11)="caption"
    uIOptions$(12)="recl"
    uIOptions$(13)="position"
    mat IncludeCols$(0) ! We're including all cols
    mat Records(0) ! We're including all records

    library : Fnreadlayouts,fnShowData,fnGetFileNumber, fnLog, fnLogArray
 !
 SELECTDATAFILE: ! ***** Datafile Selection Routine #Autonumber# 7000,10

    fnReadLayouts(Mat Lvdir_Listing$)
    fnCheckForNewExtension(mat LvDir_Listing$)
    fnIgnoreLayouts(mat LvDir_Listing$)
    
    fnReadScreenS(ScreenRows,ScreenCols)

    open #(Dc_Window:=Fngetfilenumber): "srow=2,scol="&str$(int((ScreenCols-39)/2))&",rows="&str$(ScreenRows-2)&",cols=39,Caption= Select File ... ENTER - View / F5 - Edit ,Border=S",display,outIn

    if Udim(Mat Lvdir_Listing$)>15 then lvdir_Widths(1)-=2 ! Make Room For Scrollbar

    _Logging=fnSettings("createlogfile")

    pr #Dc_Window, fields str$(ScreenRows-5)&",3,CC 10,/W:W,B43;"&str$(ScreenRows-5)&",27,CC 10,/W:W,B44" : "Quit","View"
    pr #Dc_Window, fields str$(ScreenRows-5)&",15,CC 10,/W:W,B47" : "Audit"
    pr #Dc_Window, fields str$(ScreenRows-2)&",10,CC 20,/W:W,B45" : "Generate Code"
    pr #Dc_Window, fields str$(ScreenRows-3)&",10,CC 20,/W:W,B46" : "New Layout Wizard"
    pr #Dc_Window, fields "1,2,LIST "&str$(ScreenRows-8)&"/37,HEADERS,/W:W" : (Mat Lvdir_Headings$, Mat Lvdir_Widths, Mat Lvdir_Fieldforms$)
    pr #Dc_Window, fields "1,2,LIST "&str$(ScreenRows-8)&"/37,=" : (Mat Lvdir_Listing$)
    pr #Dc_Window, fields "1,2,LIST "&str$(ScreenRows-8)&"/37,SORT" : 1

    if fn43 and fnSettings("filterboxfilelist")  then
       filterBox$=str$(ScreenRows-7)&",2,FILTER 37,/W:W,1,2,FULLROW,ALL"
    else
       filterBox$=str$(ScreenRows-7)&",2,SEARCH 37,/W:W,1,2"
    end if

    do
       input #Dc_Window, fields FilterBox$&";1,2,LIST "&str$(ScreenRows-8)&"/37,ROWCNT,SEL,44" : Search$,Count
       mat SelectedRecords(Count)
       input #Dc_Window, fields "1,2,LIST "&str$(ScreenRows-8)&"/37,ROWSUB,SEL,NOWAIT" : mat SelectedRecords
       input #Dc_Window, fields "1,2,LIST "&str$(ScreenRows-8)&"/37,ROWSUB,CUR,NOWAIT" : Chosen

       dc_Key=Fkey
       if Dc_Key=0 Or Dc_Key=201 then dc_Key=44 ! Ok Button
       if Dc_Key=99 then dc_Key=43 ! Esc Button
       if Dc_Key=93 then dc_Key=43
       if Dc_Key=44 then ! Ok Selected
          fnClearCache
          fnShowData(Lvdir_Listing$(Chosen),0,0,0,0,0,1,"","","","","Data Crawler",mat Records,mat IncludeCols$,mat UIOptions$,D$,D,D$,"count",D$,D$,D$,D$,D$,D,1)
          if fkey=93 then dc_key=43   ! pass windows X click back to parent
       end if
       if Dc_Key=5 then
          fnClearCache
          fnShowData(Lvdir_Listing$(Chosen),1,0,0,0,0,1,"","","","","Data Editor",mat Records,mat IncludeCols$,mat UIOptions$,D$,D,D$,"count",D$,D$,D$,D$,D$,D,1)
       end if
       if Dc_Key=45 then
          fnGenerateCode(Lvdir_listing$(Chosen))
       end if
       if Dc_Key=46 then
          fnLayoutWizard

          ! Refresh Listview
          fnReadLayouts(mat Lvdir_Listing$)
          fnIgnoreLayouts(mat LvDir_Listing$)
          pr #Dc_Window, fields "1,2,LIST "&str$(ScreenRows-8)&"/37,=" : (Mat Lvdir_Listing$)
          pr #Dc_Window, fields "1,2,LIST "&str$(ScreenRows-8)&"/37,SORT" : 1
       end if
       if Dc_Key=47 then
          fnCallAudit(mat SelectedRecords,mat lvdir_listing$)
       end if
       if fkey=93 then dc_key=43   ! pass windows X click back to parent
    loop Until Dc_Key=43
    close #Dc_Window:
 !
 ENDDATACRAWLER: ! #Autonumber# 8000,10
    if Turnguibackoff then execute "config gui off"
    if fkey=93 then
       execute "system"
    end if
    stop
 !
 ! #Autonumber# 10000,1
 ! *****************
 ! ** DATACRAWLER **
 ! *****************

    dim SearchNow$*999
    dim DcSpec$(2)*60
    dim filterF$(1)*1023,FilterF(1)

    def library Fndataedit(Filelay$;srow$,scol$,rows$,cols$,___,SRow,SCol,Rows,Cols)
       sRow=val(srow$) conv Ignore
       sCol=val(scol$) conv Ignore
       rows=val(rows$) conv Ignore
       cols=val(cols$) conv Ignore
       
       library : fnShowData
       fndataedit=fnShowData(fileLay$,1,SRow,SCol,Rows,Cols)
    fnend
    
    def library Fndatacrawler(Filelay$;srow$,scol$,rows$,cols$,___,SRow,SCol,Rows,Cols)
       sRow=val(srow$) conv Ignore
       sCol=val(scol$) conv Ignore
       rows=val(rows$) conv Ignore
       cols=val(cols$) conv Ignore

       library : fnShowData
       fndatacrawler=fnShowData(fileLay$,0,SRow,SCol,Rows,Cols)
    fnend

    def fnCalculateSize(&sRow,&sCol,&Rows,&Cols,Edit,mat Element$,Mat Filter$;___,ScreenRows,ScreenCols,MinRows,MinCols,MaxRows,MaxCols,MinSRow,MinSCol,AdjRows,AdjCols)
       fnReadScreenS(ScreenRows,ScreenCols)
       fnCalculateSizeLimits(mat Element$,Edit,ScreenRows,ScreenCols,Rows,Cols,MinRows,MinCols,MaxRows,MaxCols,MinSRow,MinSCol,mat Filter$)

       ! Size it within limits
       rows=max(min(Rows,MaxRows),MinRows)
       cols=max(min(Cols,MaxCols),MinCols)

       ! Center it if position isn't given
       if ~sRow then sRow=int((ScreenRows-Rows)/2)+1
       if ~sCol then sCol=int((ScreenCols-Cols)/2)+1

       adjRows=int((ScreenRows+MaxRows)/2)
       adjCols=int((ScreenCols+MaxCols)/2)

       ! Adjust it to fit it on screen
       sRow=max(min(sRow,AdjRows-Rows),MinsRow)
       sCol=max(min(sCol,AdjCols-Cols),MinsCol)
    fnend

    ! If Include is blank (not given), then include everything.
    def fnPrepIncludeUI$(mat IncludeUI$;___,Index,Empty)
       if fnIsEmptyS(mat IncludeUI$) then
          ! Default UI Options
          mat IncludeUI$(10)
          includeUI$(1)="quitbutton"
          includeUI$(2)="exportbutton"
          includeUI$(3)="importbutton"
          includeUI$(4)="columnsbutton"
          includeUI$(5)="deletebutton"
          includeUI$(6)="savebutton"
          includeUI$(7)="addbutton"
          includeUI$(8)="search"
          includeUI$(9)="border"
          includeUI$(10)="caption"
       end if

    fnend

    def fnIsEmptys(mat A$;___,Index,Empty)
       empty=1
       for Index=1 to udim(Mat A$)
          if A$(Index)<>"" then empty=0
       next Index
       fnIsEmptys=Empty
    fnend
    def fnIsEmpty(mat A;___,Index,Empty)
       empty=1
       for Index=1 to udim(Mat A)
          if A(Index) then empty=0
       next Index
       fnIsEmpty=Empty
    fnend
    def library fnEmpty(mat A)=fnIsEmpty(mat A)
    def library fnEmptyS(mat A$)=fnIsEmptyS(mat A$)

    def fnClearUI(mat OutputSpec$,mat InputSpec$,mat OutputData$,mat InputData$)
       mat OutputSpec$(0)
       mat OutputData$(0)
       mat InputSpec$(0)
       mat InputData$(0)
       buttonCount=0
    fnend

    dim ButtonCount,MaxButtonColumns
    def fnButtonRows
       if ButtonCount then
          fnButtonRows=int((ButtonCount-1)/MaxButtonColumns)+1
       end if
    fnend

    def fnButtonSpec$*36(ButtonFkey,row,cols;___,ButtonCol,ButtonRow)
       buttonCount+=1
       buttonRow=int((ButtonCount-1)/MaxButtonColumns)
       buttonCol=mod(ButtonCount-1,MaxButtonColumns)+1
       fnButtonSpec$=Str$(row-ButtonRow)&","&str$(cols-((8*(ButtonCol))-1))&","&"7/CC 10,/W:W,B"&str$(ButtonFkey)
    fnend

    def fnAddButton(mat OutputSpec$,mat OutputData$,rows,Cols,ButtonFkey,Data$)
       fnAddObject(mat OutputSpec$,mat OutputData$,fnButtonSpec$(ButtonFKey,rows,Cols),data$)
    fnend

    def fnAddObject(mat OutputSpec$,mat OutputData$,Spec$*40,Data$;___,Index)
       index=udim(mat OutputSpec$)+1
       mat OutputSpec$(Index)
       mat OutputData$(Index)
       outputSpec$(Index)=Spec$
       outputData$(Index)=Data$
       fnAddObject=Index
    fnend

    def fnCalculateSizeLimits(mat Element$,Edit,ScreenRows,ScreenCols,&Rows,&Cols,&MinRows,&MinCols,&MaxRows,&MaxCols,&MinSRow,&MinSCol,mat Filter$;___,Index,BCount,BCols,o_border,ButtonRows)

       ! Set Defaults
       maxRows=ScreenRows
       maxCols=ScreenCols
       minSRow=1
       minSCol=1
       minRows=3
       minCols=10
       
       for Index=1 to udim(mat Element$)
          #Select# lwrc$(trim$(Element$(Index))) #Case# "importbutton" # "addbutton" # "deletebutton" # "savebutton"
             if Edit then bCount+=1
          #Case# "columnsbutton" # "exportbutton" # "keybutton" # "quitbutton"
             bCount+=1
          #Case# "search"
             minRows+=1
             minCols=max(MinCols,20)
          #Case# "grid"
          #Case# "border" # "caption" # "recl"
             o_border=1
          #End Select#
       next Index

       if o_border then ! Expand to include border
          maxRows-=2 : maxCols-=2
          minsRow+=1 : minsCol+=1
       end if

       ! Default Size if not given
       if ~Rows then rows=MaxRows
       if ~Cols then cols=MaxCols
       
       ! calculate how many columns we need
       if bcount<5 or bcount<=(Cols-2)/8 then
          maxButtonColumns=bcount
       else
          maxButtonColumns=int((bcount-1)/2)+1
       end if

       ! Expand to include buttons
       if BCount then
          buttonRows=int((BCount-1)/MaxButtonColumns)+1
          minRows+=ButtonRows
          if ButtonRows>1 then
             bCols=MaxButtonColumns
          else
             bCols=mod((BCount-1),MaxButtonColumns)+1
          end if
          minCols=Max(MinCols,(8*(BCols-1))+10)
       end if

       ! Add a row for filters
       if ~fnIsEmptys(mat Filter$) then
          minRows+=1
       end if
    fnend

    def fnBuildUI(mat Element$,mat OS$,mat IS$,mat OD$,mat ID$,mat FilterFields$,mat FilterForm$,mat FilterCompare$,mat FilterCaption$,mat FilterDefaults$,mat FilterSubs,mat FilterValSubs,mat ColumnNames$,mat FilterKey,&GridSpec$,Edit,Rows,Cols;&o_caption,&o_border,&o_recl,&o_position,&SearchSub,___,o_search,Index,Grid$,FirstRow,FilterPosition,Sub,FilterString$,FilterEndString$)
       firstRow=1
       if fn43 and fnSettings("filterboxdata") then
          filterString$="FILTER"
          filterEndString$=",FULLROW,ALL"
       else
          filterString$="SEARCH"
          filterEndString$=""
       end if

       if ~fnIsEmptyS(mat FilterFields$) then
          firstRow+=1
          filterPosition=1
          mat FilterForm$(udim(mat FilterFields$))
          mat FilterCompare$(udim(mat FilterFields$))
          mat FilterCaption$(udim(mat FilterFields$))
          mat FilterDefaults$(udim(mat FilterFields$))
          mat FilterSubs(udim(mat FilterFields$))
          mat FilterValSubs(udim(mat FilterFields$))
          mat FilterKey(udim(mat FilterFields$))

          for Index=1 to udim(mat filterfields$)
             if len(trim$(filterfields$(Index))) then
                sub=srch(mat ColumnNames$,lwrc$(trim$(filterfields$(Index))))
                if sub>0 then
                   fnAddObject(mat OS$,mat OD$,"1,"&str$(FilterPosition)&",CR "&str$(len(FilterCaption$(Index))),FilterCaption$(Index))
                   filterPosition+=len(FilterCaption$(Index))+1
                   filterValSubs(Index)=fnAddObject(mat IS$,mat ID$,"1,"&str$(FilterPosition)&","&FilterForm$(Index),FilterDefaults$(Index))
                   filterPosition+=fnLengthOf(FilterForm$(Index))+2
                   filterSubs(Index)=sub
                end if
             end if
          next Index
       end if
       for Index=1 to udim(mat Element$)
          #Select# lwrc$(trim$(Element$(Index))) #Case# "columnsbutton"
             fnAddButton(mat OS$,mat OD$,Rows,Cols,55,"Columns")
          #Case# "exportbutton"
             fnAddButton(mat OS$,mat OD$,Rows,Cols,56,"Export")
          #Case# "importbutton"
             if Edit then let fnAddButton(mat OS$,mat OD$,Rows,Cols,57,"Import")
          #Case# "addbutton"
             if Edit then let fnAddButton(mat OS$,mat OD$,Rows,Cols,15,"Add")
          #Case# "savebutton"
             if Edit then let fnAddButton(mat OS$,mat OD$,Rows,Cols,5,"Save (F5)")
          #Case# "deletebutton"
             if Edit then let fnAddButton(mat OS$,mat OD$,Rows,Cols,16,"Delete")
          #Case# "keybutton"
             fnAddButton(mat OS$,mat OD$,Rows,Cols,4,"Jump (F4)")
          #Case# "quitbutton"
             fnAddButton(mat OS$,mat OD$,Rows,Cols,54,"Quit")
          #Case# "search"
             if exists("images\search.png") then
                searchSub=fnAddObject(mat IS$,mat ID$,str$(firstrow)&",3,"&FilterString$&" "&str$(cols-12)&",/W:W,2,1"&FilterEndString$,"")
                fnAddObject(mat OS$,mat OD$,str$(firstrow)&",1,P 1/2","images\search.png")
             else
                searchSub=fnAddObject(mat IS$,mat ID$,str$(firstrow)&",1,"&FilterString$&" "&str$(cols-10)&",/W:W,2,1"&FilterEndString$,"")
             end if
             fnAddObject(mat OS$,mat OD$,str$(firstrow)&","&str$(cols-8)&",CC 8,/W:W,B70","Refresh")
             o_search=1
          #Case# "border"
             o_border=1
          #Case# "caption"
             o_caption=1
             o_border=1
          #Case# "recl"
             o_recl=1
             o_caption=1
             o_border=1
          #Case# "position"
             o_position=1
          #End Select#
       next Index

       if Edit then
          fnAddObject(mat IS$,mat ID$,str$(FirstRow+o_search)&",1,GRID "&str$(rows-((firstrow-1)+o_search+fnButtonRows))&"/"&str$(cols)&",ROWCNT,CHG","")
          gridSpec$=str$(FirstRow+o_search)&",1,GRID "&str$(rows-((firstrow-1)+o_search+fnButtonRows))&"/"&str$(cols)
       else
          fnAddObject(mat IS$,mat ID$,str$(FirstRow+o_search)&",1,LIST "&str$(rows-((firstrow-1)+o_search+fnButtonRows))&"/"&str$(cols)&",ROWSUB,SELONE","")
          gridSpec$=str$(FirstRow+o_search)&",1,LIST "&str$(rows-((firstrow-1)+o_search+fnButtonRows))&"/"&str$(cols)
       end if
       mat ID$(udim(mat ID$)-1) ! Remove Data element for list; it has to be given as a Scalar
    fnend

    def fnPrepareWindow(&sRow,&sCol,&Rows,&Cols,Edit,mat Includebuttons$,&Caption$,&GridSpec$,mat Inputspec$,mat OutputSpec$,mat Inputdata$,mat OutputData$,&o_position,mat FilterFields$,mat FilterForm$,mat FilterCompare$,mat FilterCaption$,mat FilterDefaults$,mat FilterSubs,mat FilterValSubs,mat ColumnNames$,mat FilterKey,&Searchsub;___,o_caption,o_border,o_recl)
       fnPrepIncludeUI$(Mat Includebuttons$)

       fnCalculateSize(sRow,sCol,Rows,Cols,Edit,mat Includebuttons$,mat FilterFields$)
       fnClearUI(mat OutputSpec$,mat InputSpec$,mat OutputData$,mat InputData$)
       fnBuildUI(mat Includebuttons$,mat OutputSpec$,mat InputSpec$,mat OutputData$,mat InputData$,mat FilterFields$,mat FilterForm$,mat FilterCompare$,mat FilterCaption$,mat FilterDefaults$,mat FilterSubs,mat FilterValSubs,mat ColumnNames$,mat FilterKey,GridSpec$,Edit,Rows,Cols,o_caption,o_border,o_recl,o_position,SearchSub)

       ! if setting recl or
       if Caption$="" and o_Caption then caption$="View '"&Filelay$&"' File"
       if o_Recl then caption$=Caption$&" ... Recl="&str$(rln(datafile))
       if o_Border then
          if len(Caption$) then caption$=Caption$&","
          caption$=Caption$&"Border=S"
       end if
    fnend

    dim TextDisplay$*255
    dim BadRead$(1)*20000
    
    def fnPopulateGrid(Window,GridSpec$,Layout$,KeyMatch$*255,SearchMatch$*255,mat Records,mat IncludeColumns$,mat ColumnNames$,RecordSize,Datafile,mat Dc_Data$,mat Dc_Data,mat dc_forms$,&SearchNow$,mat Dc_Records,mat dc_DeleteRow,KeyNumber,&LastRec$,DisplaySub,&DisplayConvert$,mat FilterCompare$,mat FilterSubs,mat FilterValSubs,mat FilterKey,mat InputData$;IncludeRecordNumbers,___,SomethingFound,Or_Recordlocation,Clearflag$*1,Index,Animate,bIncludeColumns,Sub,bRecords,RecPointer,Stopreading,Action,TempKey$*255,RowCount,ReadCount,LastTime,BadReadFile)
       animate=fnSettings("animatedatacrawler")
       mat dc_Outputrow$(0) : mat dc_Records(0) : mat dc_Deleterow(0)
       clearflag$="="
       if Animate then let fnPrepareAnimation

       recordSize=udim(mat Widths)
       
       bIncludeColumns=(~fnIsEmptys(mat IncludeColumns$))
       bRecords=(~fnIsEmpty(mat Records))
       
       if len(keymatch$) and kln(datafile)>0 then
          keymatch$=rtrm$(keymatch$(1:kln(datafile)))
          restore #Datafile, search>=keymatch$:
       end if

       mat BadRead$(0) ! Clear Bad Record Reads

       do  ! Read Data
          lastTime=Timer
          do while file(datafile)<>10
             if bRecords then
                recPointer+=1
                if RecPointer<=udim(mat Records) then
                   read #datafile, using Dc_Forms$(datafile), rec=Records(RecPointer), release : Mat dc_Data$,Mat dc_Data error Ignore
                else
                   stopreading=1
                end if
             else
                read #datafile, using Dc_Forms$(datafile),release : Mat dc_Data$,Mat dc_Data error Ignore
             end if

             if ~file(datafile) and ~Stopreading then
                readCount+=1
                action=fnPassesFilter(mat dc_Data$,mat dc_Data,mat FilterCompare$,mat FilterValSubs,mat FilterSubs,mat InputData$,mat FilterKey)
                if Action=1 then
                   if ~len(Keymatch$) or KeyMatch$=fnbuildKey$(Layout$,mat dc_data$,mat dc_data,KeyNumber)(1:len(Keymatch$)) then
                      if ~len(SearchMatch$) or fnSearchIn(SearchMatch$,mat Dc_Data$,mat Dc_Data) then
                         if ~len(SearchNow$) or fnSearchIn(SearchNow$,mat Dc_Data$,mat Dc_Data) then
                            rowCount+=1
                            somethingfound=1
                            or_Recordlocation=Udim(Mat dc_Outputrow$)
                            mat dc_Outputrow$(Udim(dc_Outputrow$)+Recordsize) ! Add Room
                            mat dc_Deleterow(Udim(Mat dc_Records) + 1) ! Add Row To Delete From
                            mat dc_Records(Udim(Mat dc_Records) + 1)
                            dc_Records(Udim(Mat dc_Records))=Rec(datafile) ! Save Record Number For Later
                            if IncludeRecordNumbers then
                               dc_OutputRow$(Or_Recordlocation+IncludeRecordNumbers)=str$(rec(Datafile))
                            end if

                            if bIncludeColumns then
                               for Index=1 to udim(mat IncludeColumns$)
                                  sub=srch(mat ColumnNames$,IncludeColumns$(Index))
                                  if Sub>udim(mat dc_data$) then
                                     dc_OutputRow$(Or_RecordLocation+Index+IncludeRecordNumbers)=str$(dc_data(sub-udim(mat Dc_Data$)))
                                  else if Sub>0 then
                                     dc_OutputRow$(Or_RecordLocation+Index+IncludeRecordNumbers)=dc_data$(Sub)
                                  end if
                               next Index
                            else
                               if Udim(Mat dc_Data$) then ! Copy Strings
                                  mat dc_Outputrow$(Or_Recordlocation+1+IncludeRecordNumbers:Or_Recordlocation+IncludeRecordNumbers+Udim(Mat dc_Data$))=dc_Data$
                               end if
                               for Index=1 to Udim(Mat dc_Data) ! Copy Numerics
                                  dc_Outputrow$(Or_Recordlocation+Index+IncludeRecordNumbers+Udim(Mat dc_Data$))=Str$(dc_Data(Index))
                               next Index
                            end if
                         end if
                      end if
                   else
                      stopReading=1
                   end if
                else if Action<0 then
                   action=Action*-1

                   mat FilterF$(udim(mat dc_data$))=("")
                   mat FilterF(udim(mat dc_Data))=(0)

                   ! take the filter that failed
                   if FilterSubs(Action)>udim(mat FilterF$) then ! use filter to find the subscript
                      filterF(FilterSubs(Action)-udim(mat FilterF$))=val(inputdata$(FilterValSubs(Action))) conv Ignore
                   else
                      filterF$(FilterSubs(Action))=InputData$(FilterValSubs(Action))
                   end if
                      
                   ! use subscript to build an empty f$ and f with the data
                   ! use fnBuildKey$ and Keynum
                   if kln(datafile)>0 and ~fnIsEmpty(mat FilterF) or ~fnIsEmptyS(mat FilterF$) then
                      tempKey$=fnBuildKey$(Layout$,mat FilterF$,mat FilterF,Keynumber)
                      restore #datafile, search>=Trim$(TempKey$): nokey StopReading
                   end if
                else if Action=2 then
                   StopReading: stopReading=1 ! dont like it but its the simplest way to do it in this language.
                end if
                if Animate then
                   textDisplay$=""
                   if DisplaySub then
                      if DisplaySub=-2 then
                         textDisplay$=lpad$(str$(ReadCount),len(LastRec$)) ! Count of Rows Read
                      else if DisplaySub=-3 then
                         textDisplay$=lpad$(str$(RowCount),len(LastRec$))  ! Count of Rows Added
                      else if DisplaySub<0 then ! All others use record number
                         textDisplay$=lpad$(str$(rec(DataFile)),len(LastRec$))
                      else if DisplaySub<=udim(mat dc_Data$) then
                         textDisplay$=trim$(dc_data$(DisplaySub))
                      else
                         if ~len(DisplayConvert$) then
                            textDisplay$=str$(dc_data(DisplaySub-udim(mat dc_data$)))
                         else
                            textDisplay$=cnvrt$(DisplayConvert$,dc_data(DisplaySub-udim(mat dc_data$))) conv Ignore
                         end if
                      end if
                   end if
                   if len(trim$(LastRec$)) then
                      if len(TextDisplay$) then textDisplay$=TextDisplay$&" of "
                      textDisplay$=TextDisplay$&LastRec$
                   end if

                   fnAnimate(TextDisplay$)
                end if
             else
                if file(DataFile)=20 then
                   if fn43 or udim(mat BadRead$)<=40 then
                      mat BadRead$(udim(BadRead$)+1)
                      reread #DataFile, using "form C "&str$(rln(datafile)) : BadRead$(udim(BadRead$)) error Ignore
                   end if
                end if
             end if
          loop until Stopreading or fkey=99 or ((X$:=Unhex$(Kstat$))="6300") or Udim(Mat dc_Outputrow$)+Recordsize>1000 or (udim(mat dc_OutputRow$) and RowCount<=20 and (Timer-LastTime>2))

          pr #Window, fields GridSpec$&","&Clearflag$ : Mat dc_Outputrow$
          mat dc_Outputrow$(0) ! Clear Already Used Data To Go Back For More
          clearflag$="+"
       loop Until StopReading or File(datafile)=10 Or X$="6300" ! Until Eof Or Abort

       if udim(mat BadRead$) then
          if (2==msgbox("There were "&str$(udim(mat BadRead$))&" records that couldn't be read with the current file layout. Do you want to see them?","Some records could not be read: Debug?","yN","QST")) then
             open #(BadReadFile:=fnGetFileNumber): "name=@:badread[SESSION].out, recl="&str$(rln(datafile))&",replace", display, output
             for Index=1 to udim(mat BadRead$)
                fnProgressBar(Index/udim(mat BadRead$))
                pr #BadReadFile: BadRead$(Index)
             next Index
             close #BadReadFile:
             fnCloseBar
             execute "system -@ -C -M start "&os_filename$("@:badread[SESSION].out")
          end if
       end if
       
       if Animate then let fnCloseAnimation
       fnPopulateGrid=SomethingFound
    fnend

    def fnPassesFilter(mat dc_Data$,mat dc_Data,mat FilterCompare$,mat FilterValSubs,mat FilterSubs,mat InputData$,mat FilterKey;___,Index,Passing)
       passing=1 ! Give it a fighting chance (in case there are no filters)
       do while (Index:=Index+1)<=udim(mat FilterSubs)
          if filterSubs(Index)>0 then
             if Len(FilterCompare$(Index)) then
                passing=0
                if pos(FilterCompare$(Index),"*") then ! Fuzzy Search
                   if FilterSubs(Index)>udim(Mat dc_Data$) then
                      if len(trim$(InputData$(FilterValSubs(Index)))) and fnIsnumber(trim$(InputData$(FilterValSubs(Index)))) then
                         if pos(str$(dc_Data(FilterSubs(Index)-udim(mat Dc_Data$))),trim$(InputData$(FilterValSubs(Index)))) then
                            ! It passes this filter
                            passing=1
                         else
                            ! Failed numeric filter test
                         end if
                      else
                         passing=1
                      end if
                   else
                      if len(trim$(InputData$(FilterValSubs(Index)))) then
                         if pos(dc_Data$(FilterSubs(Index)),trim$(InputData$(FilterValSubs(Index)))) then
                            ! It passes this filter
                            passing=1
                         else
                            ! Failed numeric filter test
                         end if
                      else
                         passing=1
                      end if
                   end if
                end if
                if pos(FilterCompare$(Index),">") then
                   if FilterSubs(Index)>udim(Mat dc_Data$) then
                      if len(trim$(InputData$(FilterValSubs(Index)))) and fnIsnumber(trim$(InputData$(FilterValSubs(Index)))) then
                         if dc_Data(FilterSubs(Index)-udim(mat Dc_Data$))>val(InputData$(FilterValSubs(Index))) then
                            ! It passes this filter
                            passing=1
                         else
                            ! Failed numeric filter test
                         end if
                      else
                         passing=1
                      end if
                   else
                      if len(trim$(InputData$(FilterValSubs(Index)))) then
                         if dc_Data$(FilterSubs(Index))>InputData$(FilterValSubs(Index)) then
                            ! It passes this filter
                            passing=1
                         else
                            ! Failed numeric filter test
                         end if
                      else
                         passing=1
                      end if
                   end if
                end if
                if pos(FilterCompare$(Index),"<") then
                   if FilterSubs(Index)>udim(Mat dc_Data$) then
                      if len(trim$(InputData$(FilterValSubs(Index)))) and fnIsnumber(trim$(InputData$(FilterValSubs(Index)))) then
                         if dc_Data(FilterSubs(Index)-udim(mat Dc_Data$))<val(InputData$(FilterValSubs(Index))) then
                            ! It passes this filter
                            passing=1
                         else
                            ! Failed numeric filter test
                         end if
                      else
                         passing=1
                      end if
                   else
                      if len(trim$(InputData$(FilterValSubs(Index)))) then
                         if dc_Data$(FilterSubs(Index))<InputData$(FilterValSubs(Index)) then
                            ! It passes this filter
                            passing=1
                         else
                            ! Failed numeric filter test
                         end if
                      else
                         passing=1
                      end if
                   end if
                end if
                if pos(FilterCompare$(Index),"=") then
                   if FilterSubs(Index)>udim(Mat dc_Data$) then
                      if len(trim$(InputData$(FilterValSubs(Index)))) and fnIsnumber(trim$(InputData$(FilterValSubs(Index)))) then
                         if dc_Data(FilterSubs(Index)-udim(mat Dc_Data$))=val(InputData$(FilterValSubs(Index))) then
                            ! It passes this filter
                            passing=1
                         else
                            ! Failed numeric filter test
                         end if
                      else
                         passing=1
                      end if
                   else
                      if len(trim$(InputData$(FilterValSubs(Index)))) then
                         if trim$(dc_Data$(FilterSubs(Index)))=trim$(InputData$(FilterValSubs(Index))) then
                            ! It passes this filter
                            passing=1
                         else
                            ! Failed numeric filter test
                         end if
                      else
                         passing=1
                      end if
                   end if
                end if
             end if
          end if
       loop while Passing

       ! Index will be the one it failed on if it failed
       if ~Passing then ! Check filter key and return action
          if FilterKey(Index)=1 then
             passing=Index*-1
          else if FilterKey(Index)=-1 then
             passing=2
          end if
       end if

       fnPassesFilter=Passing
    fnend

    dim GridSpec$*60
    dim OutputSpec$(1)*60
    dim OutputData$(1)
    dim InputSpec$(1)*60
    dim InputData$(1)*255
    dim Widths(1)
    dim Description$(1)*255
    dim ColumnNames$(1)*255
    
    dim FilterSubs(1)
    dim FilterValSubs(1)
    
    dim IncludeColumns$(1)
    dim Includebuttons$(1)
    dim FieldPositions(1)
    
    dim LastRec$*255
    dim DisplayFieldConvert$*255
    
    def library fnShowData(FileLay$*128;Edit,sRow,sCol,Rows,Cols,KeyNumber,Caption$*127,Path$*255,KeyMatch$*255,SearchMatch$*255,CallingProgram$*255,mat Records,mat IncludeCols$,mat IncludeUI$,mat ColumnDescription$,mat ColumnWidths,mat ColumnForms$,DisplayField$*80,mat FilterFields$,mat FilterForm$,mat FilterCompare$,mat FilterCaption$,mat FilterDefaults$,mat FilterKey,IncludeRecordNumbers,___,Window,Index,X$,Key,Key$,Rowcount,Shouldsave,Saveindex,Saveloop,Saverecordsub,Recordsize,Deleteindex,Rebuildcolor,Changedrowcount,Oldchangeddatasize,Oldchangedsubssize,Somethingfound,Logging,Animate,InputFlag,Sub,IncludeCol,TestConvert,RecordNum,o_position,DisplaySub,SearchSub,AlreadyWarned,MergeIndex,MergeFound,MergeFromStart,MergeFromEnd,MergeToStart,MergeToEnd)
       library : Fngetfilenumber, fnOpenFile, fnCloseFile, fnreadlayoutarrays, fnbuildKey$, fnSetLogChanges, fnLogChanges, fnLog, fnLogArray, fnProgressBar, fnCloseBar

       mat FilterSubs(0)
       mat FilterValSubs(0)

       if CallingProgram$="" then callingProgram$="FileIO's fnShowData"
       logging=fnSettings("createlogfile")

       if Logging then
          if Edit then
             fnLog("Edit "&trim$(FileLay$)&" file.",CallingProgram$)
          else
             fnLog("View "&trim$(FileLay$)&" file.",CallingProgram$)
          end if
       end if

       ! Make copies of any optional arrays we need to change
       ! in case they aren't given and we can't change the original
       mat IncludeColumns$(udim(mat IncludeCols$))=IncludeCols$
       mat Includebuttons$(udim(mat IncludeUI$))=IncludeUI$
       searchNow$=""

       if ~keynumber then KeyNumber=-1 ! Default to Relative for preformance
       if Edit and ~fn42 then keyNumber=1 ! Must open keyed due to bug in 4.1 and earlier

       datafile=Fnopen(Filelay$,Mat dc_Data$,Mat dc_Data,Mat Dc_Forms$,(~Edit),KeyNumber,0,path$,Mat dc_Dfdescr$,Mat dc_Widths)

       if datafile then

          ! Return field names to be used later when populating grid
          fngetfieldnames(Filelay$,mat ColumnNames$,mat FieldPositions)

          fnLwrcThis(mat IncludeColumns$)
          fnLwrcthis(mat IncludeCols$)
          fnLwrcThis(mat ColumnNames$)

          fnPrepareWindow(sRow,sCol,Rows,Cols,Edit,mat Includebuttons$,Caption$,GridSpec$,mat Inputspec$,mat OutputSpec$,mat Inputdata$,mat OutputData$,o_Position,mat FilterFields$,mat FilterForm$,mat FilterCompare$,mat FilterCaption$,mat FilterDefaults$,mat FilterSubs,mat FilterValSubs,mat ColumnNames$,mat FilterKey,SearchSub)

          open #(Window:=Fngetfilenumber) : "sRow="&str$(srow)&",sCol="&str$(scol)&",Rows="&str$(rows)&",Cols="&str$(cols)&",Caption="&Caption$,display,outIn

          pr #Window, fields mat OutputSpec$ : mat OutputData$

          do  ! Whole Thing

             lastRec$=DisplayField$
             if len(DisplayField$) then
                if lwrc$(displayfield$)="rec" then
                   lastRec$=str$(lrec(DataFile))
                   displaySub=-1
                else if lwrc$(displayfield$)="count" or lwrc$(displayfield$)="readcount" then
                   lastRec$=str$(lrec(DataFile))
                   displaySub=-2
                else if lwrc$(displayfield$)="findcount" then
                   displaySub=-3
                else
                   if (sub:=srch(mat ColumnNames$,lwrc$(trim$(displayfield$))))>0 then
                      displaySub=Sub
                   end if
                end if
             end if

             fncalculatefieldforms(Filelay$,mat ColumnNames$,Mat Fieldforms$,mat Widths,mat Description$,Mat dc_Widths,Mat dc_Dfdescr$,mat IncludeColumns$,mat IncludeCols$,mat ColumnDescription$,mat ColumnWidths,mat ColumnForms$,mat FieldPositions,o_position,DisplaySub,DisplayFieldConvert$)

             if DisplaySub>0 then

                read #datafile, using dc_forms$(Datafile), last : mat dc_data$, mat dc_data error Ignore

                if file(datafile)=0 then

                   if DisplaySub>udim(mat dc_data$) then
                      if DisplaySub<=udim(mat dc_Data$)+udim(mat dc_Data) then
                         if ~len(DisplayFieldConvert$) then
                            lastRec$=str$(dc_data(DisplaySub-udim(mat dc_data$)))
                         else
                            lastRec$=cnvrt$(DisplayFieldConvert$,dc_data(DisplaySub-udim(mat dc_data$))) conv Ignore
                         end if
                      end if
                   else
                      lastRec$=dc_data$(DisplaySub)
                   end if
                end if
             end if
             
             If IncludeRecordNumbers then
                mat Widths(udim(mat Widths)+1)
                mat Description$(udim(mat Description$)+1)
                mat FieldForms$(udim(mat FieldForms$)+1)
                for Index=udim(mat Widths) to 2 step -1
                   widths(Index)=Widths(Index-1)
                   description$(Index)=Description$(Index-1)
                   fieldForms$(Index)=FieldForms$(Index-1)
                next Index
                widths(1)=5
                description$(1)="Rec"
                fieldForms$(1)="N 16"
             end if
                
             recordsize=Udim(Mat Widths)
             colsChanged=0
             mat dc_Blank$(Recordsize)
             pr #Window, fields GridSpec$&",HEADERS,/W:W" : (Mat Description$,Mat Widths,Mat Fieldforms$)

             changedrowcount=0 : mat dc_Changeddata$(0) : mat dc_Changedsubs(0)

             restore #datafile:
             if kln(datafile)>0 and trim$(Key$)<>"" then
                restore #datafile, search>=Trim$(Key$): nokey ClearKey
             end if
             if RecordNum then
                restore #datafile, rec=RecordNum: noRec ClearRec
             end if

             somethingFound=fnPopulateGrid(Window,GridSpec$,FileLay$,KeyMatch$,SearchMatch$,mat Records,mat IncludeColumns$,mat ColumnNames$,RecordSize,Datafile,mat Dc_Data$,mat Dc_Data,mat dc_forms$,SearchNow$,mat Dc_Records,mat dc_DeleteRow,KeyNumber,LastRec$,DisplaySub,DisplayFieldConvert$,mat FilterCompare$,mat FilterSubs,mat FilterValSubs,mat FilterKey,mat InputData$,IncludeRecordNumbers)
             alreadyWarned=0

             pr #Window, fields mat InputSpec$(1:udim(mat InputSpec$)-1) : mat InputData$

             do  ! Input Data
                if RebuildColor then gosub UPDATECOLORS
                if Somethingfound then
                   if udim(mat InputData$) then
                      input #Window, fields mat InputSpec$ : mat InputData$, Rowcount
                   else
                      input #Window, fields mat InputSpec$ : Rowcount
                   end if
                   key=Fkey
                   if Key=54 then key=99
                else
                   if Edit and (2==Msgbox("There is no data in the file, or no records match the current search criteria. Would you like to add a row?","File Empty","YN","EXCL")) then
                      key=15 ! Add A Row
                      somethingfound=1
                   else
                      if ~AlreadyWarned then let msgbox("Data File is Empty or no records match the current search criteria.","File Empty") : AlreadyWarned=1
                      if udim(mat InputData$) then
                         input #Window, fields mat InputSpec$(1:udim(mat InputSpec$)-1) : mat InputData$
                         key=Fkey
                         if Key=54 then key=99
                      else
                         key=99
                      end if
                   end if
                end if
                shouldsave=0

                #Select# Key #Case# 15 and Edit
                   gosub MergeNewChanges   ! Get any additional changes since the last time

                   pr #Window, fields GridSpec$&",+" : Mat dc_Blank$ ! Add A Blank Row
                   mat dc_Records(Udim(Mat dc_Records)+1)
                   mat dc_Deleterow(Udim(Mat dc_Records))
                   curfld(1,((Udim(Mat dc_Records)-1)*Recordsize) + 1)

                #Case# 16 and Edit
                   dc_Deleterow(Currow)=dc_Deleterow(Currow)+1 ! Toggle Deleted
                   rebuildcolor = 1

                #Case# 55
                   if Changedrowcount Or (Edit and Rowcount) or Sum(Mat dc_Deleterow) then NeedsSave=1 else NeedsSave=0
                   colsChanged=fnSelectDataCrawlerCols(mat dc_Dfdescr$,mat IncludeColumns$,mat ColumnNames$,rows,cols,NeedsSave)
                   if ColsChanged = 2 then ShouldSave = 2

                #Case# 56
                   fnCSV_Export(FileLay$,Keynumber,Key$,RecordNum,mat Records,SearchMatch$,KeyMatch$,SearchNow$)
                #Case# 57 and Edit
                   fnCSV_Import(FileLay$)

                #Case# 4 # 99 # 93 # 70
                   if Edit and (Changedrowcount Or Rowcount Or Sum(Mat dc_Deleterow)) then
                      shouldsave=Msgbox("All unsaved changes will be lost. Do you want to Save first?","Search Warning","YNC","EXCL")
                   end if
                #End Select#
             loop Until ((Key=99 or Key=93 Or Key=4 or Key=70) And Shouldsave<>4) Or Key=5 or Key=57 or (Key=55 And ColsChanged) ! F4 Changes Settings, F5 Saves Settings, 70 is Search, needs to redraw

             if Edit and (Key=5 Or Shouldsave=2) then gosub SaveChangedData

             if Key=4 then let Fninputkey(Key$,RecordNum,datafile,rows,cols,Window)

             if key=70 then
                searchNow$=trim$(InputData$(SearchSub))  ! If there's a search box, its always first cause the grids always last so this should work but it would be better to use a subscript.
                ! inputData$(SearchSub)=""
             end if

          loop Until Key=99 or Key=93 or fkey=93

          if Edit then inputFlag=1 else inputFlag=-1
          fnclosefile(datafile,filelay$,path$,InputFlag)
          if Logging then let fnLog("Close "&trim$(FileLay$)&" file.",CallingProgram$)
       end if
       close #Window:
    fnend

 ClearKey: ! Invalid Key Entered
    msgbox("Key Not Found")
    key$=""
    continue

 ClearRec: ! Invalid Record Entered
    msgbox("Record not found")
    recordNum=0
    continue
    
 Tryagain: ! Try Again
    if fkey=99 or fkey=93 then
       continue
    else
       retry
    end if
    
 DECONVERSIONFAILED: ! Conversion Failed Saving Numeric Field
    testconvert=dc_Data(sub-udim(mat Dc_data$)) ! Keep Existing Value, New Value Failed
    continue

 MergeNewChanges: ! Save new changes into the Changed Subs and Data arrays
    if Rowcount then ! If There Are Already Changes Made, Save Them To Backup Arrays
       mat dc_Savedata$(Recordsize*Rowcount) ! Make Room
       mat dc_Savesubs(Rowcount) ! Make Room
       input #Window, fields GridSpec$&",ROWSUB,CHG,NOWAIT" : Mat dc_Savesubs
       input #Window, fields GridSpec$&",ROW,CHG,NOWAIT" : Mat dc_Savedata$

       saveRecordSize=RecordSize-IncludeRecordNumbers

       for MergeIndex=1 to udim(mat dc_SaveSubs)
          mergeFromStart=((MergeIndex-1)*RecordSize)+1+IncludeRecordNumbers
          mergeFromEnd=MergeIndex*RecordSize

          mergeFound=srch(mat dC_ChangedSubs,dc_SaveSubs(MergeIndex))
          if MergeFound>0 then
             ! Find the Data to Update
             mergeToStart=((MergeFound-1)*SaveRecordSize)+1
             mergeToEnd=MergeFound*SaveRecordSize
          else
             ! Add Rows to Update
             mergeToStart=udim(mat Dc_ChangedData$)+1
             mergeToEnd=udim(mat Dc_changedData$)+SaveRecordSize

             mergeFound=udim(mat Dc_ChangedSubs)+1

             mat Dc_ChangedData$(MergeToend)
             mat Dc_ChangedSubs(MergeFound)
          end if

          ! Merge It
          mat dc_ChangedData$(MergeToStart:MergeToEnd)=dc_SaveData$(MergeFromStart:MergeFromEnd)
          dc_ChangedSubs(MergeFound)=dc_SaveSubs(MergeIndex)
       next MergeIndex

       changedrowcount=Udim(Mat dc_Changedsubs)
    end if
 return

 dim SaveMessage$*255,UpdateCount,AddCount,DeleteCount
 SaveChangedData: ! Save all the changed records in the data crawler
    saveMessage$="" : updateCount=0 : addCount=0 : deleteCount=0

    gosub MergeNewChanges   ! Get any additional changes since the last time
    saveRecordSize=RecordSize-IncludeRecordNumbers

    if (~fnIsEmptys(mat IncludeColumns$)) then includeCol=1
    ! Save Prerecorded Changes
    for Saveloop = 1 to Changedrowcount
       saverecordsub=((Saveloop-1)*SaveRecordSize)
       if dc_Deleterow(dc_Changedsubs(Saveloop))=0 then ! Skip It If Its Deleted
          mat dc_Data$=("") : mat Dc_Data=(0)
          if dc_Records(dc_Changedsubs(Saveloop)) then
             read #datafile, using Dc_Forms$(datafile), rec=dc_Records(dc_Changedsubs(Saveloop)) : Mat dc_Data$, Mat dc_Data
             if Logging then let fnSetLogChanges(mat dc_Data$,mat dc_Data)
          end if

 !        Convert Strings Back To Numbers And Assign Back To File Arrays
          for SaveIndex=1 to SaveRecordSize
             if IncludeCol then
                sub=srch(mat ColumnNames$,IncludeColumns$(SaveIndex))
             else
                sub=SaveIndex
             end if

             if Sub>udim(mat dc_data$) then
                testConvert=val(dc_ChangedData$(SaveRecordSub+saveindex)) conv DeConversionFailed
                dc_data(sub-udim(mat Dc_data$))=TestConvert
             else if Sub>0 then
                dc_data$(sub)=dc_ChangedData$(SaveRecordSub+SaveIndex)
             end if
          next SaveIndex

 !        Save The Data
          if dc_Records(dc_Changedsubs(Saveloop)) then
             rewrite #datafile, using Dc_Forms$(datafile) : Mat dc_Data$, Mat dc_Data
             if Logging then let fnLogChanges(mat dc_Data$,mat dc_Data,"Update """&FileLay$&""" Record: "&str$(rec(datafile)),CallingProgram$,FileLay$)
             updateCount+=1
          else
             write #datafile, using Dc_Forms$(datafile) : Mat dc_Data$, Mat dc_Data
             if Logging then let fnLogArray(Mat dc_Data$,mat dc_Data,"Write """&FileLay$&""" Record: "&str$(rec(datafile))&" Data: ",CallingProgram$)
             addCount+=1
          end if
       end if
    next Saveloop

    ! Delete the records that need to be deleted
    for Deleteindex=1 to Udim(Mat dc_Deleterow)
       if dc_Deleterow(Deleteindex) then
          if dc_Records(Deleteindex) then
             delete #datafile, rec=dc_Records(Deleteindex):
             if Logging then let fnLog("Delete """&FileLay$&""" Record: "&str$(dc_Records(Deleteindex)),CallingProgram$)
             deleteCount+=1
          end if
          dc_Deleterow(Deleteindex)=2 ! Clear Them To White Next Round
       end if
    next Deleteindex
    if Edit then gosub UPDATECOLORS
    crlf$=Hex$("0D0A")
    msgbox("Update Complete!"&Crlf$&Crlf$&str$(AddCount)&" records Added"&Crlf$&str$(UpdateCount)&" records Updated"&Crlf$&str$(DeleteCount)&" records Deleted","Update")
 return

 !  Save New Changes
 !   mat dc_Savedata$(Recordsize*Rowcount) ! Make Room
 !   mat dc_Savesubs(Rowcount)             ! Make Room
 !   input #Window, fields GridSpec$&",ROWSUB,CHG,NOWAIT" : Mat dc_Savesubs
 !   input #Window, fields GridSpec$&",ROW,CHG,NOWAIT" : Mat dc_Savedata$

 !  One Record At A Time
 !   for Saveloop = 1 to Rowcount
 !      saverecordsub=((Saveloop-1)*Recordsize)
 !      if dc_Deleterow(dc_Savesubs(Saveloop))=0 then ! Skip It If Its Deleted
 !         mat dc_Data$=("") : mat Dc_Data=(0)
 !         if dc_Records(dc_Savesubs(Saveloop)) then
 !            read #datafile, using Dc_Forms$(datafile), rec=dc_Records(dc_Savesubs(Saveloop)) : Mat dc_Data$, Mat dc_Data
 !            if Logging then let fnSetLogChanges(mat dc_Data$,mat dc_Data)
 !         end if

 !         for SaveIndex=1 to RecordSize
 !            if IncludeCol then
 !               sub=srch(mat ColumnNames$,IncludeColumns$(SaveIndex))
 !            else
 !               sub=SaveIndex
 !            end if

 !            if Sub>udim(mat dc_data$) then
 !               testConvert=val(dc_Savedata$(SaveRecordSub+saveindex)) conv DeConversionFailed
 !               dc_data(sub-udim(mat Dc_data$))=TestConvert
 !            else if Sub>0 then
 !               dc_data$(sub)=dc_Savedata$(SaveRecordSub+SaveIndex)
 !            end if
 !         next SaveIndex

 !        Save The Data
 !         if dc_Records(dc_Savesubs(Saveloop)) then
 !            rewrite #datafile, using Dc_Forms$(datafile) : Mat dc_Data$, Mat dc_Data
 !            if Logging then let fnLogChanges(mat dc_Data$,mat dc_Data,"Update """&FileLay$&""" Record: "&str$(rec(datafile)),CallingProgram$,FileLay$)
 !         else
 !            write #datafile, using Dc_Forms$(datafile) : Mat dc_Data$, Mat dc_Data
 !            if Logging then let fnLogArray(Mat dc_Data$,mat dc_Data,"Write """&FileLay$&""" Record: "&str$(rec(datafile))&" Data: ",CallingProgram$)
 !         end if
 !      end if
 !   next Saveloop
    

 SELECTDATACRAWLERCOLS: ! **** Open Window to Select Display Columns for Data Crawler ****
    def fnselectdatacrawlercols(mat Field_Desc$,mat IncludeColumns$,mat ColumnNames$,Rows,Cols;NeedsSave,___,sRow,sCol,ScreenRows,ScreenCols,Row,Col,fK,CloseWindow,ShouldSave,ItemsPerpage,Index,Jndex)
       dim Input_Spec$(1)*255,Input_Data$(1)*1000

       fnReadScreenS(ScreenRows,ScreenCols)

       ! Grow to minimum reasonable size
       rows=max(rows,8)
       cols=max(cols,48)
       
       rows=min(ScreenRows,Rows)
       cols=min(ScreenCols,Cols)

       mat Input_Spec$(UDim(mat Field_Desc$))
       mat Input_Data$(UDim(mat Field_Desc$))
        
       ! Open main window
       sRow=Int((ScreenRows-Rows) / 2) + 1
       sCol=Int((ScreenCols-Cols) / 2) + 1
        
       open #(ColSel_Window:=Fngetfilenumber): "SROW="&Str$(SRow)&", SCOL="&Str$(SCol)&", ROWS="&str$(rows)&", COLS="&str$(cols),display,outIn
       pr #ColSel_Window, fields str$(rows)&","&str$(cols-22)&",CC 10,/W:W,B55" : "OK"
       pr #ColSel_Window, fields str$(rows)&","&str$(cols-10)&",CC 10,/W:W,B54" : "Cancel"
       pr #ColSel_Window, fields str$(rows)&","&str$(cols-34)&",CC 10,/W:W,B56" : "Select All"
       pr #ColSel_Window, fields str$(rows)&","&str$(cols-46)&",10/CC 11,/W:W,B57" : "Select None"
       if cols>60 then
          pr #ColSel_Window, fields str$(rows)&",2,"&str$(cols-49)&"/C 55" : "Choose data columns to display"
       end if

       dim ColSel_Tabs(1)
       dim TabAttr$*50

       itemsPerpage=(rows-4)*int(Cols/19)

       row = 2
       col = 2
       thisTab = 0
       For Index = 1 To UDim(mat Input_Spec$)
          If Row = 2 And Col = 2 Then  ! Open a child window
             thisTab+=1
             mat ColSel_Tabs(ThisTab)
             if UDim(mat Input_Spec$) > ItemsPerpage then TabAttr$=", TAB=Page "&Str$(ThisTab) else TabAttr$=", BORDER=S, CAPTION=Column List"
             open #(ColSel_Tabs(ThisTab):=Fngetfilenumber): "SROW=2, SCOL=2, ROWS="&str$(rows-3)&", COLS="&str$(cols-2)&TabAttr$&", PARENT="&Str$(ColSel_Window),display,outIn
          End If
        
          input_Spec$(Index)=Str$(Row)&","&Str$(Col)&",18/check 1000,,"&str$(1500+Index)
          input_Data$(Index)=Field_Desc$(Mod(Index-1,UDim(mat Field_Desc$))+1)
            
          if fnIsEmptys(mat IncludeColumns$) or srch(mat IncludeColumns$,ColumnNames$(Index))>0 then
             input_Data$(Index) = "^"&Input_Data$(Index)
          end if

          row+=1
          If Row > Rows-3 And Col+18+19 > Cols Then
             row = 2
             col = 2
          Else If Row > Rows-3 Then
             col +=19
             row = 2
          End If
       Next Index
        
       do
          do
             thisTab = CURTAB(ColSel_Tabs(1))
             For TabIndex = 1 To UDim(ColSel_Tabs)
                If ColSel_Tabs(TabIndex) = ThisTab Then CurrentTabIndex=TabIndex
             Next TabIndex
             startCol = 1 + (CurrentTabIndex - 1) * ItemsPerpage
             endCol = Min(CurrentTabIndex * ItemsPerpage, UDim(Input_Spec$))
             rinput #ThisTab, fields mat Input_Spec$(StartCol:EndCol) : mat Input_Data$(StartCol:EndCol)
             fk=fkey
             
             if fK=56 then ! Select All
                for Index=1 to udim(mat Input_Data$)
                   input_Data$(Index)=trim$(Input_Data$(Index))
                   if Input_Data$(Index)(1:1)<>"^" then
                      input_Data$(Index)(999:1000)=""
                      input_Data$(Index)(1:0)="^"
                   end if
                next Index
             else if fK=57 then ! Select None
                for Index=1 to udim(mat Input_Data$)
                   if Input_Data$(Index)(1:1)="^" then
                      input_Data$(Index)(1:1)=""
                   end if
                next Index
             end if
          loop Until fK=0 or fK=99 or fK=93 or fK=54 or fK=55 ! 0,55: OK; 99,93,54: Cancel

          closeWindow=1
          if fK=0 or fK=55 then
             if NeedsSave then
                shouldsave=Msgbox("All unsaved changes will be lost. Do you want to Save first?","Search Warning","YNC","EXCL")
                if ShouldSave=2 then fnSelectDataCrawlerCols=2
                if ShouldSave=3 then fnSelectDataCrawlerCols=1
                if ShouldSave=4 then CloseWindow=0
             else
                fnSelectDataCrawlerCols=1
             end if
             if CloseWindow then
                mat IncludeColumns$(0)
                for Index = 1 To UDim(Input_Data$)
                   if Input_Data$(Index)(1:1)="^" then
                      jndex=udim(mat IncludeColumns$)+1
                      mat IncludeColumns$(Jndex)
                      includeColumns$(Jndex)=ColumnNames$(Index)
                   end if
                next Index
                if udim(mat IncludeColumns$)=udim(mat Input_Data$) then
                   mat IncludeColumns$(0)
                end if
             end if
          else
             fnselectdatacrawlercols=0
          end if
       loop Until CloseWindow
        
       For ThisTab = 1 To UDim(ColSel_Tabs)
          close #(ColSel_Tabs(ThisTab)):
       Next ThisTab

       close #ColSel_Window:
    fnend

 def library fnCsvImport(Layout$*64;SupressDialog,FileName$*300,ImportModeKey,___,OkToRun)
    if SupressDialog then
       if trim$(Filename$)><"" and exists(trim$(FileName$)) then
          okToRun=1
       else
          if trim$(FileName$)="" then
             msgbox("The output file name was not given.","File Not Passed","Ok","ERR")
          else
             msgbox("File ("&trim$(Filename$)&") could not be found.","File not Found","Ok","ERR")
          end if
       end if
    else
       if fnCSVImportDlg(Layout$,Filename$,ImportModeKey) then
          okToRun=1
       end if
    end if

    if OkToRun then
       fnImportFromCSV(Layout$,Filename$,ImportModeKey)
    end if
 fnend

 def library fnCsvExport(Layout$*64;SupressDialog,Filename$*300,IncludeRecNums,KeyNumber,StartKey$,KeyMatch$,Startrec,mat Records,SearchMatch$,___,SearchNow$,OkToRun)
    library : fnReadLayoutHeader,fnReadLayoutArrays,fnOpenFile, fnGetFileNumber
    if SupressDialog then
       if trim$(Filename$)><"" then
          okToRun=1
       else
          msgbox("The output file name was not given.","File Not Passed","Ok","ERR")
       end if
    else
       if fnCsvExportDlg(Filename$,IncludeRecNums) then
          okToRun=1
       end if
    end if

    if OkToRun then
       fnExportToCSV(Layout$,Filename$,IncludeRecNums,KeyNumber,StartKey$,StartRec,mat Records,SearchMatch$,KeyMatch$,SearchNow$,SupressDialog)
    end if
 fnend

 def fnCSV_Import(Layout$*64;___,Filename$*300,UseKey)
    library : fnReadLayoutHeader,fnReadLayoutArrays,fnOpenFile, fnGetFileNumber
    if fnCSVImportDlg(Layout$,Filename$,UseKey) then
       fnImportFromCSV(Layout$,Filename$,UseKey)
    end if
 fnend

 def fnCSV_Export(Layout$*64;Keynumber,&Key$,&RecordNum,mat Records,&SearchMatch$,&KeyMatch$,&SearchNow$,___,Filename$*300,IncludeRecords)
    if fnCsvExportDlg(Filename$,IncludeRecords) then
       fnExportToCSV(Layout$,Filename$,IncludeRecords,KeyNumber,Key$,RecordNum,mat Records,SearchMatch$,KeyMatch$,SearchNow$)
    end if
 fnend


 dim CsvFieldNames$(1)*128,CSVFieldPositions(1)
    
 EXPORTTOCSV: ! **** Export a data file to CSV format
    def fnExportToCSV(Layout$*64,CSVFile$*300,UseRecNums;Keynumber,&Key$,&RecordNum,mat Records,&SearchMatch$,&KeyMatch$,&SearchNow$,SuppressMsg,___,CSVFile,Datafile,bRecNum)
       library : fnReadLayoutHeader,fnReadLayoutArrays,fnOpenFile, fnGetFileNumber

       OPEN #(CSVFile:=fngetfilenumber): "name="&CSVFile$&",recl=32000,replace",DISPLAY,OUTPUT IOERR IGNORE

       if File(CSVFile) = 0 then
          ! pr field names in first line
          fnGetFieldNames(Layout$,mat CsvFieldNames$,mat CSVFieldPositions)

          if UseRecNums then pr #CSVFile: "&RECNUM,";
          for FInd = 1 to UDim(CsvFieldNames$)
             if FInd < UDim(CsvFieldNames$) then pr #CSVFile: CsvFieldNames$(FInd);","; else pr #CSVFile: CsvFieldNames$(FInd)
          next FInd

          Datafile=fnOpen(Layout$,mat Csv_Data$,mat Csv_Data,mat Csv_Forms$,1,Keynumber)

          if kln(datafile)>0 and trim$(Key$)<>"" then
             restore #datafile, search>=Trim$(Key$): nokey ClearKey
          end if
          if RecordNum then
             restore #datafile, rec=RecordNum: noRec ClearRec
          end if

          bRecNum=(~fnIsEmpty(mat Records))

          do
             read #Datafile, using Csv_Forms$(Datafile), release : Mat Csv_Data$,Mat Csv_Data error Ignore
             
             if File(DataFile)=0 then
                if ~bRecNum or srch(mat Records,rec(datafile))>0 then
                   if ~len(Keymatch$) or KeyMatch$=fnbuildKey$(Layout$,mat Csv_Data$,mat Csv_Data,Keynumber)(1:len(Keymatch$)) then
                      if ~len(SearchMatch$) or fnSearchIn(SearchMatch$,mat Csv_Data$,mat Csv_Data) then
                         if ~len(SearchNow$) or fnSearchIn(SearchNow$,mat Csv_Data$,mat Csv_Data) then
                            if UseRecNums then pr #CSVFile: STR$(REC(Datafile))&",";
                            for FInd = 1 to UDim(CsvFieldNames$)
                               if FInd <= UDim(Csv_Data$) then
                                  pr #CSVFile: """"&SRep$(Csv_Data$(FInd),"""","""""")&"""";
                               else
                                  pr #CSVFile: Csv_Data(FInd - UDim(Csv_Data$));
                               end if
                               if FInd < UDim(CsvFieldNames$) then pr #CSVFile: ","; else pr #CSVFile:
                            next FInd
                         end if
                      end if
                   end if
                end if
             end if
          loop until file(datafile)=10

          close #Datafile:
          close #CSVFile:

          if ~SuppressMsg then let msgbox("Export complete!")
       else
          MsgBox("Error: could not open output file!")
       end if
    fnend
 !
    def Fnclientserver(;___,Serverfolder$*255,Clientfolder$*255)
        if wbversion$(1:3)>="4.3" then
           if lwrc$(trim$(env$("br_model")))="client/server" then
              fnClientServer=1
           else
              fnClientServer=0
           end if
        else
           serverfolder$=Os_Filename$(".")
           clientfolder$=Os_Filename$("@:.")
           if Clientfolder$(Len(Clientfolder$):Len(Clientfolder$))<>"\" then
              clientfolder$=Clientfolder$&"\"
           end if
           if Serverfolder$(Len(Serverfolder$):Len(Serverfolder$))<>"\" then
              serverfolder$=Serverfolder$&"\"
           end if
           if Lwrc$(Trim$(Serverfolder$))=Lwrc$(Trim$(Clientfolder$)) then
              fnclientserver=0
           else
              fnclientserver=1
           end if
        end if
    fnend

 def fnCSVExportDlg(&CSVPath$,&UseRecNums;___,Dialog,KeyP,CSVFile,UseRecNums$*50,FileString$*255,Rows,Cols)
     library : fnReadLayoutHeader

     dim Keys$(1)*80,KeyDesc$(1)*80,SrcFieldNames$(1)*80
     fnReadScreenS(Rows,Cols)

     OPEN #(Dialog:=fnGetFileNumber) : "SROW="&STR$(INT((Rows - 7) / 2))&",SCOL="&STR$(INT((Cols - 30) / 2))&",ROWS=7,COLS=30,CAPTION=Export to CSV,BORDER=S",display,outIn
     pr #Dialog, fields "1,1,C 20;2,22,CC 8,/W:W,B62" : "CSV Data File","Browse"
     pr #Dialog, fields "7,12,CC 8,/W:W,B61" : "Export!"
     pr #Dialog, fields "7,22,CC 8,/W:W,B60" : "Cancel"
     if fnClientServer then
        fileString$="@:CSV files (*.csv) |*.csv"
     else
        fileString$="CSV files (*.csv) |*.csv"
     end if
     
     if len(trim$(CSVPath$)) then
        open #(CSVFile:=fnGetFileNumber): "name="&CSVPath$&",recl=32000,new",display,output,error Ignore
        if file(CSVFile)=0 then
           ! Path is good
        else
           if file(CSVFile)<>-1 then close #CSVFile:
           OPEN #(CSVFile:=fngetfilenumber): "name=SAVE:"&CSVPath$&",recl=32000,replace",DISPLAY,OUTPUT IOERR IGNORE
        end if
     else
        OPEN #(CSVFile:=fngetfilenumber): "name=SAVE:"&FileString$&",recl=32000,replace",DISPLAY,OUTPUT IOERR IGNORE
     end if

     if File(CSVFile) = 0 then
         CSVPath$=File$(CSVFile)
         close #CSVFile:
         UseRecNums$ = "Include Record Numbers"
         if UseRecNums then useRecNums$(1:0)="^"
         do
             RINPUT #Dialog, fields "2,1,20/C 300,P;4,1,CHECK 50" : CSVPath$,UseRecNums$

             KeyP=FKey
             
             if KeyP=62 then
                 ! browse for a new file
                 OPEN #(CSVFile:=fngetfilenumber): "name=SAVE:"&FileString$&",recl=32000,replace",DISPLAY,OUTPUT IOERR IGNORE
                 if File(CSVFile) = 0 then
                     CSVPath$=File$(CSVFile)
                     close #CSVFile:
                 end if
             end if
         loop Until KeyP = 60 or KeyP = 61 or KeyP = 99 or KeyP=93
         
         if UseRecNums$(1:1)="^" then UseRecNums=1 else UseRecNums=0
         
         if KeyP = 61 then
             ! do export
             fnCsvExportDlg=1
         end if
     end if
     
     CLOSE #Dialog:
 fnend


 IMPORTFROMCSV: ! **** Import data from an external CSV file
 def fnImportFromCSV(Layout$,&Filename$,KeyNum;___,Datafile,RecNum$,CharInd,NumQ,CSVFile,Indx)
     library : fnBuildKey$,fnCloseFile
 
     dim CSVLine$*32000,CSVLineBuf$*32000
     dim SrcFieldNames$(1)*80

     OPEN #(CSVFile:=fngetfilenumber): "name="&Filename$,DISPLAY,INPUT IOERR IGNORE
     if File(CSVFile) = 0 then
         RESTORE #CSVFile:
         LINPUT #CSVFile: CSVLine$
     
         fieldCnt=fnExtractCSVHeader(CSVLine$,mat SrcFieldNames$)
         fnLwrcThis(mat SrcFieldNames$)

         fnGetFieldNames(Layout$,mat CsvFieldNames$,mat CSVFieldPositions)

         ! Make the comparison case insensitive by lwrcing everything
         for Indx=1 to udim(mat SrcFieldNames$)
            srcFieldNames$(Indx)=lwrc$(SrcFieldNames$(Indx))
         next Indx
         for Indx=1 to udim(mat CsvFieldNames$)
            csvFieldNames$(Indx)=lwrc$(CsvFieldNames$(Indx))
         next Indx

         if KeyNum > 0 then
             datafile=Fnopen(Layout$,Mat Csv_Data$,Mat Csv_Data,Mat Csv_Forms$,0,KeyNum)
         else
             datafile=Fnopen(Layout$,Mat Csv_Data$,Mat Csv_Data,Mat Csv_Forms$)
         end if

         fnClearCSVApplyMessages

         do
             CSVLine$=""
             do
                 LINPUT #CSVFile: CSVLineBuf$ ERROR IGNORE
                 CSVLine$=CSVLine$&CSVLineBuf$
             
                 ! Special case: starting from the end of the line, if there are an odd number of double quotes before the last comma,
                 ! the line is ending with a newline inside string data.  Therefore, grab an additional line
                 numQ=0
                 for CharInd = LEN(CSVLine$) to 1 STEP -1
                     if CSVLine$(CharInd:CharInd)="""" then NumQ+=1
                     if CSVLine$(CharInd:CharInd)="," then CharInd=0
                 next CharInd
             loop until Mod(NumQ,2)=0
             if File(CSVFile) = 0 then
                 fnExtractCSVLine(CSVLine$,mat Csv_Data$,mat Csv_Data,mat CsvFieldNames$,mat SrcFieldNames$)

                 if fnDisplayCSVApplyMessages then
                     ! There were no errors, or they chose Ignore
                     if KeyNum > 0 then
                         ! attempt to rewrite
                         REWRITE #DataFile, USING Csv_Forms$(Datafile), Key=fnBuildKey$(Layout$, mat Csv_Data$, mat Csv_Data, KeyNum) : mat Csv_Data$, mat Csv_Data ERROR IGNORE
                         if File(DataFile) = 21 then
                             WRITE #Datafile, USING Csv_Forms$(Datafile) : mat Csv_Data$, mat Csv_Data
                         end if
                     else if KeyNum = 0 then
                         RecNum$=CSVLine$(1:POS(CSVLine$,",")-1)
                         REWRITE #DataFile, USING Csv_Forms$(Datafile), Rec=VAL(RecNum$) : mat Csv_Data$, mat Csv_Data ERROR IGNORE
                         if File(DataFile) = 21 then
                             WRITE #Datafile, USING Csv_Forms$(Datafile) : mat Csv_Data$, mat Csv_Data
                         end if
                     else
                         WRITE #Datafile, USING Csv_Forms$(Datafile) : mat Csv_Data$, mat Csv_Data
                     end if
                 else
                     ! They chose abort
                     exit do
                 end if
             end if
         loop Until File(CSVFile) <> 0

         fnCloseFile(Datafile,Layout$)
     else
         msgbox("Error "&str$(err)&" Opening File "&Filename$,"File Error","Ok","Err")
     end if
 fnend

 def fnCSVImportDlg(Layout$,&CSVPath$,&KeyNum;___,Dialog,Key,FileName$*255,CSVPath2$*300,Key$*160,Index,Sel$,CSVFile,CSVLine$*32000,FileChanged,FileString$*255,Rows,Cols,CurrentField,FunctionKey)
     library : fnReadLayoutHeader
 
     dim Keys$(1)*80,KeyDesc$(1)*80,KeyOptions$(1)*160,SrcFieldNames$(1)*80
     fnReadScreenS(Rows,Cols)

     OPEN #(Dialog:=fnGetFileNumber) : "SROW="&STR$(INT((Rows - 7) / 2))&",SCOL="&STR$(INT((Cols - 30) / 2))&",ROWS=7,COLS=30,CAPTION=Import from CSV,BORDER=S",display,outIn
     pr #Dialog, fields "1,1,C 20;2,22,CC 8,/W:W,B62" : "CSV Data File","Browse"
     pr #Dialog, fields "7,22,CC 8,/W:W,B60" : "Cancel"

     fnReadLayoutHeader(Layout$,FileName$,mat Keys$,mat KeyDesc$,0)
     
     if fn42 then Sel$=",SELECT" else Sel$=""
     if len(Trim$(CSVPath$))=0 then
        if Pos(FileName$,".") > 0 then CSVPath$=FileName$(1:POS(FileName$,"."))&"csv" else CSVPath$=FileName$&".csv"
     end if
     FileChanged = 1
     do
         ! Check whether selected file exists and if so, look for &RECNUM field
         CSVPath2$=CSVPath$
         if FileChanged then
             FileChanged = 0
             if File(CSVFile) >< -1 then CLOSE #CSVFile:
             OPEN #(CSVFile=fngetfilenumber): "name="&CSVPath$,DISPLAY,INPUT IOERR IGNORE
         end if
         if File(CSVFile) = 0 then  ! File exists and is open
             pr #Dialog, fields "4,1,C 26" : "Update Records Using Key"
             
             RESTORE #CSVFile:
             LINPUT #CSVFile: CSVLine$
             fnExtractCSVHeader(CSVLine$,mat SrcFieldNames$)
             if lwrc$(trim$(SrcFieldNames$(1),"&")) = "recnum" then
                 mat KeyOptions$(UDim(KeyDesc$) + 2)
                 KeyOptions$(2)="Record Number"
             else
                 mat KeyOptions$(UDim(KeyDesc$) + 1)
             end if
             KeyOptions$(1)="None (append all records)"
             for Index = 1+UDim(KeyOptions$)-UDim(KeyDesc$) to UDim(KeyOptions$)
                  KeyOptions$(Index) = STR$(Index+UDim(KeyDesc$)-UDim(KeyOptions$))&" - "&Keys$(Index+UDim(KeyDesc$)-UDim(KeyOptions$))&" ("&KeyDesc$(Index+UDim(KeyDesc$)-UDim(KeyOptions$))&")"
             next Index
             pr #Dialog, fields "5,1,26/COMBO 160,= "&Sel$ : mat KeyOptions$

             if KeyNum=-1 then
                key$=KeyOptions$(1)
             else if KeyNum=0 and udim(mat KeyOptions$)>=2 and KeyOptions$(2)="Record Number" then
                key$=KeyOptions$(1)
             else
                if udim(mat KeyOptions$)>=2 and KeyOptions$(2)="Record Number" then
                   key$=KeyOptions$(KeyNum+2)
                else
                   key$=KeyOptions$(KeyNum+1)
                end if
             end if

             curfld(CurrentField,FunctionKey)
             RINPUT #Dialog, fields "2,1,20/C 300,AEX;5,1,26/COMBO 160,"&Sel$ : CSVPath$,Key$
             functionKey=fkey
             currentField=Curfld
         else                  ! File doesn't exist; don't show any other input fields
             Key$=""
             pr #Dialog, fields "7,11,CC 8" : ""
             pr #Dialog, fields "4,1,C 26;5,1,C 26" : "",""
             RINPUT #Dialog, fields "2,1,20/C 300,AEX" : CSVPath$
         end if
         Key=FKey
         
         if Key = 62 then
             ! try to open file by browsing
             FileChanged = 0
             if File(CSVFile) >< -1 then CLOSE #CSVFile:

             if fnClientServer then
                fileString$="@:CSV files (*.csv) |*.csv"
             else
                fileString$="CSV files (*.csv) |*.csv"
             end if

             OPEN #(CSVFile=fngetfilenumber): "name=OPEN:"&FileString$,DISPLAY,INPUT IOERR IGNORE
             if File(CSVFile) = 0 then
                CSVPath$=File$(CSVFile)
             else
                FileChanged = 1
             end if
         end if
         if CSVPath2$<>CSVPath$ then
             ! retry file using contents of text box
             FileChanged = 1
         end if
         if Key$="" then
            pr #Dialog, fields "7,12,CC 8" : ""
         else
            pr #Dialog, fields "7,12,CC 8,/W:W,B61" : "Import!"
         end if

     loop until Key = 60 or Key = 61 or Key = 99 or Key=93
     
     if Key = 61 then
         ! get data key number; -1 = append all, 0 = use recnum
         if Key$="Record Number" then
             KeyNum=0
         else if Key$(1:4)="None" then
             KeyNum=-1
         else
             KeyNum=Val(Key$(1:Pos(Key$," ")-1))
         end if

         ! do the import
         fnCSVImportDlg=1
     end if
     if file(CSVFile)><-1 then close #CSVFile:
     close #Dialog:
 fnend

 EXTRACTCSVHEADER: ! Extract field names from a CSV header line into an array
 def fnExtractCSVHeader(&Line$,mat FieldNames$)
     Line$=TRIM$(Line$)
     ChunkStart=1 : FieldCnt=0
     for CharInd = 1 to LEN(Line$)
         if Line$(CharInd:CharInd)="," then
             FieldCnt = FieldCnt + 1
             mat FieldNames$(FieldCnt)
             if CharInd=ChunkStart then FieldNames$(FieldCnt)="" else FieldNames$(FieldCnt)=Line$(ChunkStart:CharInd-1)
             ChunkStart=CharInd+1
         else if CharInd=LEN(Line$) then
             FieldCnt = FieldCnt + 1
             mat FieldNames$(FieldCnt)
             FieldNames$(FieldCnt)=Line$(ChunkStart:CharInd)
         end if
     next CharInd
     fnExtractCSVHeader=FieldCnt
 fnend

 def fnExtractCSVLine(&Line$,mat SValues$,mat NValues,mat DestFieldNames$,mat SrcFieldNames$;___,ChunkStart,CurField,CharInd,Chunk$*4000,NumInd,StrInd)
      line$=TRIM$(Line$)
      chunkStart=1 : curField=1
      mat SValues$=("")
      mat NValues=(0)

      for CharInd = 0 to LEN(Line$)
          if Line$(CharInd+1:CharInd+1)="," or CharInd=LEN(Line$) then
              ! grab chunk to parse
              chunk$=rtrm$(Line$(ChunkStart:CharInd)) ! RTRM, BR doesn't store trailing spaces anyway.
              nextField = 0

              ! pr line$ : pr chunk$ : pause ! Debug line

              if Chunk$(1:1)<>"""" or Mod(fnNumEndingQuotes(Chunk$),2)=1 then
                 ! Either has no beg qoute (not quoted) or has both
                 ! beginning and ending qoute, process it. Otherwise, go
                 ! back for more.
              
                 if Chunk$(1:1)="""" and Mod(fnNumEndingQuotes(Chunk$),2)=1 then
                    chunk$=SREP$(Chunk$(2:LEN(Chunk$)-1),"""""","""") ! Substitute out inner Qoutes, and Strip out Qoutes
                 end if
                 ! if len(Trim$(Chunk$)) then ! If there's something left ! Commented out by GSB .. if a field is empty, we want to clear the value, not ignore it.
                 fnApplyCSVField(Chunk$,mat SValues$,mat NValues,mat DestFieldNames$,SrcFieldNames$(CurField))
                 ! end if
                 nextField = 1
              end if

              if NextField then
                  ! CharInd=CharInd+2
                  ChunkStart=CharInd +2
                  CurField = CurField + 1
              end if
          end if
      next CharInd
 fnend

    ! Old Logic here replaced by the above code.
    ! Old Logic for Numbers (Without beginning quote):
    !  for NumInd=UDim(SValues$)+1 to UDim(SValues$)+Udim(NValues)
    !     if DestFieldNames$(NumInd)=SrcFieldNames$(CurField) then NValues(NumInd - UDim(SValues$))=Val(Chunk$)
    !  next NumInd

    ! Old Logic for Strings (With Beginning Qoute):
    !  for StrInd=1 to UDim(SValues$)
    !     if DestFieldNames$(StrInd)=SrcFieldNames$(CurField) then SValues$(StrInd)=Chunk$
    !  next StrInd

    def fnApplyCSVField(&SourceData$,mat SValues$,mat NValues, mat DestFieldNames$,&SourceFieldName$;___,Index)
       index=srch(mat DestFieldNames$,SourceFieldName$)
       if Index>=1 and Index<=udim(SValues$)+udim(NValues) then
          if Index>Udim(SValues$) then
             if fnIsNumber(Chunk$) then
                nValues(Index-udim(SValues$))=val(Chunk$)
             else
                if ~ErrorImportMessage then ! If not asked yet, then ask.
                   ! Conversion Error, Field was not a number when it should have been
                   mat FieldsConv$(udim(Mat FieldsConv$)+1)
                   fieldsConv$(udim(mat FieldsConv$))=SourceFieldName$
                end if
             end if
          else
             sValues$(Index)=Chunk$
          end if
       else
          ! Field could not be found, report it later
          if SourceFieldName$<>"&recnum" then
             mat FieldsMissing$(udim(FieldsMissing$)+1)
             fieldsMissing$(udim(FieldsMissing$))=SourceFieldName$
          end if
       end if
    fnend

    dim FieldsMissing$(1)*127
    dim FieldsConv$(1)*127

    def fnClearCSVApplyMessages
       mat FieldsMissing$(0)
       mat FieldsConv$(0)
    fnend

    dim MessageText$*4000
    def fnDisplayCSVApplyMessages
       if udim(mat FieldsMissing$)+udim(mat fieldsconv$) then ! If there were errors,
          fnBuildMessageText(MessageText$,mat FieldsMissing$,mat FieldsConv$)
          if (2==msgbox(MessageText$,"Import Error","Yn","ERR")) then
             fnDisplayCSVApplyMessages=0
          else
             fnDisplayCSVApplyMessages=1
          end if
       else
          fnDisplayCSVApplyMessages=1
       end if
    fnend

    def fnBuildMessageText(&MessageText$,mat Missing$,mat Conv$;___,Index,np$,nl$)
       np$=hex$("0D0A0D0A")
       nl$=hex$("0D0A")

       messageText$="Errors were encountered during the import process."
       if udim(mat Missing$) then
          messageText$=MessageText$&np$&"The following fields could not be found in the data file and will be ignored if you continue:"&nl$
          for Index=1 to udim(mat Missing$)
             messageText$=MessageText$&Missing$(Index)&", "
          next Index
          messagetext$=MessageText$(1:len(MessageText$)-2) ! Chop off final ","
       end if
       if udim(mat Conv$) then
          messageText$=MessageText$&np$
          if udim(mat Missing$) then
             messageText$=MessageText$&"Additionally, t"
          else
             messageText$=MessageText$&"T"
          end if
          messageText$=MessageText$&"he following fields were numeric fields that contained string data, and will be ignored if you continue:"&nl$
          for Index=1 to udim(mat Conv$)
             messageText$=MessageText$&Conv$(Index)&", "
          next Index
          messagetext$=MessageText$(1:len(MessageText$)-2) ! Chop off final ","
       end if
       messagetext$=MessageText$&np$&"Do you wish to Stop the Import process?"
       fnClearCSVApplyMessages
    fnend

    ! that delicate nervous shake
    ! upon discovering a pause
    ! left behind by mistake
    ! like a sock in the kaverns of kroz

    def fnNumEndingQuotes(Chunk$*4000;___,CharInd,QuoteCnt)
       QuoteCnt = 0
       for CharInd = LEN(Chunk$) to 2 STEP -1
          if Chunk$(CharInd:CharInd)="""" then QuoteCnt += 1
       next CharInd
       fnNumEndingQuotes = QuoteCnt
    fnend

    ! discovered in some dusty function
    ! that you thought was always working
    ! abandoned like a lost conjunction
    ! a dangerous pause, secretly lurking

    def fnLwrcThis(mat A$;___,Index)
       for Index=1 to udim(mat A$)
          a$(Index)=lwrc$(A$(Index))
       next Index
    fnend

    ! what is this pause
    ! and why is it there?
    ! why is it forgotten,
    ! abandoned somewhere?

 !
 dim outColumnNames$(1)*255
 CALCULATEFIELDFORMS: ! ***** Calculate Field Forms From Widths And Num Of Strings/Numerics
    def Fncalculatefieldforms(layout$,mat ColumnNames$,Mat Fieldforms$,mat outWidths,mat outDescription$,Mat Widths,Mat Description$,Mat IncludeColumns$,mat OriginalIncludeColumn$,mat ColumnDescription$,mat ColumnWidths,mat ColumnForms$,mat FieldPositions;bShowPositions,displaySub,&DisplayFieldConvert$,___,Index,CompressColumns,MaxColWidth,Sub,bColumnDescription,bColumnWidth,bOriginalInclude,bColumnForms)

       ! Prepare width calculation
       compressColumns=fnSettings("compresscolumns")
       maxColWidth=fnSettings("maxcolwidth")

       ! Prepare Column Descriptions and Widths
       bColumnWidth=~fnIsEmpty(mat ColumnWidths)
       bColumnForms=~fnIsEmptys(mat ColumnForms$)
       bColumnDescription=~fnIsEmptys(mat ColumnDescription$)
       bOriginalInclude=~fnIsEmptyS(mat OriginalIncludeColumn$)

       if ~fnISEmptys(mat IncludeColumns$) then
          ! Size temp arrays
          mat outWidths(udim(mat IncludeColumns$))
          mat outDescription$(udim(mat IncludeColumns$))
          mat outColumnNames$(udim(mat IncludeColumns$))

          ! Sort through IncludeColumns and place columns in the grid in order.
          for Index=1 to udim(mat IncludeColumns$)
             sub=srch(mat ColumnNames$,IncludeColumns$(Index))
             if Sub>0 then  ! found it
                outWidths(Index)=Widths(Sub)
                outDescription$(Index)=Description$(Sub)
                if bShowPositions then outDescription$(Index)(1:0)=str$(FieldPositions(Sub))&" - "
                outColumnNames$(Index)=IncludeColumns$(Index)
             end if
          next Index
       else
          mat outWidths(udim(mat Widths))=Widths
          mat outDescription$(udim(mat Description$))
          mat outColumnNames$(udim(mat ColumnNames$))=ColumnNames$
          for Index=1 to udim(Mat Description$)
             outDescription$(Index)=Description$(Index)
             if bShowPositions then outDescription$(Index)(1:0)=str$(FieldPositions(Index))&" - "
          next Index
       end if

       mat Fieldforms$(Udim(Mat outWidths))
       for Index=1 to Udim(Mat Fieldforms$)
          fieldforms$(Index)="C "&Str$(outWidths(Index))
          if bColumnDescription then
             if bOriginalInclude then
                ! interpret ColumnDescription and ColumnWidth as matching original include
                sub=srch(mat OriginalIncludeColumn$,outColumnNames$(Index))
                if Sub>0 and Sub<=udim(mat ColumnDescription$) then outDescription$(Index)=ColumnDescription$(Sub)
             else
                ! interpret columndescription and Columnwidth as matching file record
                if Index<udim(mat ColumnDescription$) then
                   outDescription$(Index)=ColumnDescription$(Index)
                end if
             end if
          end if

          if ~CompressColumns then
             outWidths(Index)=Max(outWidths(Index),Len(outDescription$(Index)))
          end if
          outWidths(Index)=Min(outWidths(Index),MaxColWidth)

          if bColumnForms then
             if bOriginalInclude then
                ! interpret ColumnForms as matching original include
                sub=srch(mat OriginalIncludeColumn$,outColumnNames$(Index))
                if Sub>0 and Sub<=udim(mat ColumnForms$) and len(trim$(ColumnForms$(Sub))) then fieldForms$(Index)=ColumnForms$(Sub)
             else
                ! interpret ColumnForms as matching file record
                if Index<udim(mat ColumnForms$) and len(trim$(ColumnForms$(Index))) then
                   fieldForms$(Index)=ColumnForms$(Index)
                end if
             end if
          end if
          
          if bColumnWidth then
             if bOriginalInclude then
                ! interpret ColumnDescription and ColumnWidth as matching original include
                sub=srch(mat OriginalIncludeColumn$,outColumnNames$(Index))
                if Sub>0 and Sub<=udim(mat ColumnWidths) then outWidths(Index)=ColumnWidths(Sub)
             else
                ! interpret columndescription and Columnwidth as matching file record
                if Index<=udim(mat ColumnWidths) then
                   outWidths(Index)=ColumnWidths(Index)
                end if
             end if
          end if
          
       next Index
       
       if DisplaySub>0 and DisplaySub<=udim(mat ColumnNames$) then
          sub=srch(mat outColumnNames$,ColumnNames$(DisplaySub))
          if Sub>0 then
             displayFieldConvert$=FieldForms$(Sub)
          end if
       end if
    fnend

 GETFIELDNAMES: ! Get the field names from the layout file in strings first, numerics last order
    def fnGetFieldNames(Layout$*255,mat FieldNames$,mat Positions)
       dim StringSubs$(1)*80,NumSubs$(1)*80,StringSpecs$(1)*80,NumSpecs$(1)*80,StringDesc$(1)*80,NumDesc$(1)*80,StringPos(1),NumPos(1)
       fnReadLayoutArrays(Layout$,prefix$,mat StringSubs$,mat NumSubs$,mat StringSpecs$,mat NumSpecs$,mat StringDesc$,mat NumDesc$,mat StringPos,mat NumPos,0)
       subCnt=UDim(StringSubs$)+UDim(NumSubs$)
       mat FieldNames$(SubCnt)
       if udim(StringSubs$) then mat FieldNames$(1:udim(StringSubs$))=StringSubs$
       if udim(mat NumSubs$) then mat FieldNames$(udim(StringSubs$)+1:udim(mat FieldNames$))=NumSubs$
       mat Positions(SubCnt)
       if udim(mat StringPos) then mat Positions(1:udim(mat StringPos))=StringPos
       if udim(mat NumPos) then mat Positions(udim(mat StringPos)+1:udim(mat Positions))=NumPos
   fnend

 !
 dim InputKeyField$(2)
 ! #Autonumber# 13000,10
 INPUTKEY: ! ***** Prompts The User For The Key To Look Up In The Datafile
    def Fninputkey(&Key$,&RecordNum,Datafile,rows,cols;parentwindow,___,Window,Field$,srow,scol,viewlength,Length,Height,RecordLen)
       height=1
       recordLen=len(str$(lrec(datafile)))+1
       if kln(Datafile)>0 then
          ! height+=1
          length=kln(Datafile)
       end if
       
       ! Uncomment the Hight code above and the Record num code below to
       !  enable both at the same time here. They also need to be enabled
       !  both at the same time in fnShowData by opening file twice,
       !  and using the correct file channel to search by key or by
       !  record number
       
       srow=int((rows-Height)/2)+1
       viewlength=min(cols-2,max(length,RecordLen)+4)
       viewlength=max(viewlength,15)
       scol=int((cols-viewlength)/2)+1
       open #(Window:=Fngetfilenumber) : "srow="&str$(srow)&",scol="&Str$(scol)&",rows="&str$(Height)&",cols="&Str$(viewlength)&",Caption= Jump to ... ,Border=S,Parent="&str$(parentwindow),display,output

       mat InputKeyField$(Height)
       if kln(DataFile)>0 then
          ! pr #Window, fields "1,1,CR 4;2,1,CR 4" : "Key:","Rec:"
          pr #Window, fields "1,1,CR 4" : "Key:"
          inputKeyField$(1)="1,6,"&str$(min(ViewLength-6,Length))&"/C "&Str$(Length)
          ! inputKeyField$(2)="2,6,"&str$(min(ViewLength-6,RecordLen))&"/N "&Str$(RecordLen)
          rinput #Window, fields mat InputKeyField$ : Key$ ! ,RecordNum conv Tryagain
       else
          pr #Window, fields "1,1,CR 4" : "Rec:"
          inputKeyField$(1)="1,6,"&str$(min(ViewLength-6,RecordLen))&"/N "&Str$(RecordLen)
          input #Window, fields mat InputKeyField$ : RecordNum conv Tryagain
       end if
       if fkey=99 or fkey=93 then recordNum=0 : key$=""
       
       close #Window:
    fnend
 !
 UPDATECOLORS: ! ***** Update Listview Colors
    mat dc_Colorstart(0) !:
    mat dc_Colorend(0) !:
    mat dc_Color$(0) ! Clear Color Markings
    for Deleteindex=1 to Udim(Mat dc_Deleterow)
       if dc_Deleterow(Deleteindex) then ! If Row Is Marked For Deletion, Show It In Red
          mat dc_Colorstart(Udim(Mat dc_Colorstart)+1) !:
          mat dc_Colorend(Udim(Mat dc_Colorstart)) !:
          mat dc_Color$(Udim(Mat dc_Colorstart)) ! Make Room !:
          dc_Colorstart(Udim(Mat dc_Colorstart)) = (((Deleteindex-1) * Recordsize)+1) !:
          dc_Colorend(Udim(Mat dc_Colorstart)) = (Deleteindex * Recordsize)
          if dc_Deleterow(Deleteindex)=1 then !:
             dc_Color$(Udim(Mat dc_Colorstart)) = "/#FF0000:#FFFF00" ! Red With Yellow Background !:
          else !:
             dc_Color$(Udim(Mat dc_Colorstart)) = "/#000000:#FFFFFF" ! Black With White Background !:
             dc_Deleterow(Deleteindex)=0
       end if
    next Deleteindex
    if (Udim(Mat dc_Colorstart)) then !:
       pr #Window, fields GridSpec$&",ATTR" : (Mat dc_Colorstart, Mat dc_Colorend, Mat dc_Color$)
    return
 !

 dim SearchFor$*999
 def fnSearchIn(&Search$,mat F$,mat F;___,SearchChunk$*64,Index,Found,Number,Passing)
    searchFor$=Search$

    do while len(SearchFor$)
       if pos(SearchFor$," ") then
          searchChunk$=lwrc$(trim$(SearchFor$(1:pos(SearchFor$," "))))
          searchFor$(1:pos(SearchFor$," "))=""
          searchFor$=trim$(SearchFor$)
       else
          searchChunk$=lwrc$(trim$(SearchFor$))
          searchFor$=""
       end if
    
       found=0
       do
          for Index=1 to udim(mat F$)
             if pos(lwrc$(F$(Index)),SearchChunk$) then
                found=1
                exit do      ! We're done searching as soon as we found it anywhere
             end if
          Next Index
          if fnIsNumber(SearchChunk$(1:120)) then
             for Index=1 to udim(mat f)
                if pos(str$(f(Index)),SearchChunk$) then
                   found=1
                   exit do   ! We're done searching as soon as we found it anywhere
                end if
             next Index
          end if
       loop until 1
    loop while Found         ! Every chunk has to be found for it to be legit.
    fnSearchIn=Found
 fnend
 
 def fnIsNumber(String$*120;___,TestNumber)
    testNumber=val(String$) Conv NotNumber
    fnIsNumber=1
    NotNumber: ! Not a number
 fnend

 ! #Autonumber# 15000,2
 dim TemplateProg$(1)*255,TemplateDesc$(1)*255,TemplateNum(1),TemplateChoice$*255
 GenerateCode: ! This function generates code if code templates are available
    def fnGenerateCode(FileLay$;___,TemplateChoice)
       library : fnAskCombo$
       mat TemplateProg$(0)
       mat TemplateDesc$(0)

       ! Read the Templates Directory
       fnReadTemplates(mat TemplateProg$,mat TemplateDesc$,mat TemplateNum)

       if udim(mat TemplateDesc$) then
          templateChoice$=fnAskCombo$(mat TemplateDesc$,"Select Template")

          ! code templates are library functions. They can do their own input/output.
          templateChoice=srch(mat TemplateDesc$,TemplateChoice$)
          if TemplateChoice>0 then
             library TemplateProg$(TemplateChoice) : fnTemplateList, fnRunTemplate
             fnRunTemplate(TemplateNum(TemplateChoice),fileLay$)
          end if
          
       else
          msgbox("No valid templates were found.","No Templates")
       end if
    fnend
    
    def library fnAskCombo$*255(mat Description$;Caption$*60,Default$*255,___,Window,Choice$*255,ScreenRows,ScreenCols,SRow,SCol)
       library : fnGetFileNumber
       if Caption$="" then caption$="Select"

       fnReadScreenS(ScreenRows,ScreenCols)

       sRow=int((ScreenRows-3)/2)+1
       sCol=int((ScreenCols-20)/2)+1

       ! Open child window
       open #(Window:=Fngetfilenumber): "SROW="&str$(srow)&", SCOL="&str$(scol)&", ROWS=3, COLS=20, Border=S, Caption="&Caption$,display,outIn

       choice$=Default$

       ! allow user to select code template
       pr #Window, fields "3,7,CC 7,/W:W,B0" : "Ok"
       pr #Window, fields "1,2,17/COMBO 255,=,SELECT" : mat Description$
       rinput #Window, fields "1,2,17/COMBO 255,/W:W,SELECT" : Choice$

       close #Window:
       fnAskCombo$=Choice$
    fnend

    ! Define Listviews
    dim Layoutwizard_LV1_Captions$(1), Layoutwizard_LV1_Widths(1), Layoutwizard_LV1_Specs$(1)
    dim Layoutwizard_LV1_Spec$*40, LayoutWizard_LV1_PopSpec$*40

    ! Declare Variables
    dim lw_Output_Spec$(17)*60,lw_Output_Data$(17)*60
    dim lw_Input_Spec$(5)*255,lw_Input_Data$(5)*1000
    dim OpenString$(1)*999
 LayoutWizard: ! This function gives a dialog from which programmers can quickly generate file layouts
    def fnLayoutWizard(;___,Window,ScreenRows,ScreenCols,SRow,SCol,ExitMode,Generate,Function,FileBrowser,Index,Jndex,Curfield)

       fnReadScreenS(ScreenRows,ScreenCols)
       
       sRow=int((ScreenRows-22)/2)+1
       sCol=int((ScreenCols-60)/2)+1

       ! Read Specs for Output Controls
       LayoutWizardData: restore LayoutWizardData ! Restore Data Statements

       read mat lw_Output_Spec$
       data "22,44,14/CC 15,/W:W,B1504","2,1,11/C 12"
       data "7,1,13/C 15","12,1,13/C 15"
       data "22,27,14/CC 6,/W:W,B1508","17,1,13/C 15"
       data "1,1,8/CR 9","1,41,8/CC 6,/W:W,B1513"
       
       data "1,51,8/CC 8,/W:W,B1514","2,16,8/CC 4,/W:W,B1515"
       data "7,16,8/CC 4,/W:W,B1516","12,16,8/CC 4,/W:W,B1517"
       data "17,16,8/CC 4,/W:W,B1518","2,27,8/CC 5,/W:W,B1520"
       data "2,41,8/CC 9,/W:W,B1521","2,51,8/CC 8,/W:W,B1522"
       data "22,4,17/CC 16,/W:W,B1523"

       read mat lw_Output_Data$
       data "Generate Layout","Open String:","Read Statement:","Form Statement:"
       data "Done","Dim Statements:","Program:","Browse"
       data "Search","Scan","Scan","Scan","Scan"
       data "Paste","Clear All","Scan All"
       data "Open Scratch Pad"

       ! Read Specs for Input Controls
       read mat lw_Input_Spec$
       data "1,10,28/V 999,S/W:W"
       data "3,1,GRID 4/60,ROW,ALL"
       data "8,1,240/V 999,^ENTER_CRLFS/W:W","13,1,240/V 999,^ENTER_CRLFS/W:W"
       data "18,1,240/V 999,^ENTER_CRLFS/W:W"

       read Layoutwizard_LV1_Spec$
       data "3,1,GRID 4/60,HEADERS,/W:W"
       read Layoutwizard_LV1_Pop$
       data "3,1,GRID 4/60,="
       
       ! Fix for earlier versions of BR that don't support ENTER_CRLFS
       if ~fn42ia then
          for Index=1 to udim(mat lw_Input_Spec$)
             lw_Input_Spec$(Index)=srep$(lw_Input_Spec$(Index),"^ENTER_CRLFS","")
          next Index
       else if ~fn42j then
          for Index=1 to udim(mat lw_Input_Spec$)
             lw_Input_Spec$(Index)=srep$(lw_Input_Spec$(Index),"^ENTER_CRLFS","^ENTER-CRLFS")
          next Index
       end if

       read mat Layoutwizard_LV1_Captions$
       data "Open String"
       read mat Layoutwizard_LV1_Widths
       data 40
       read mat Layoutwizard_LV1_Specs$
       data "V 999,E"

       read lw_Generate,lw_Done,lw_Browse,lw_ScanProgram,lw_ScanOpen,lw_ScanRead,lw_ScanForm,lw_ScanDim,lw_PasteOpen,lw_ClearAll,lw_ScanAll,lw_OpenScratchPad
       data 1504,1508,1513,1514,1515,1516,1517,1518,1520,1521,1522,1523

       read lw_ProgramName,lw_ReadString,lw_FormString,lw_DimString
       data 1,2,3,4

       fnBuildSrchHelp(1)

       ! Open Screen
       open #(Window:=Fngetfilenumber): "SROW="&str$(srow)&", SCOL="&str$(scol)&", ROWS=22, COLS=60, Border=S, Caption=File Layout Wizard",display,outIn

       pr #Window, fields mat lw_Output_Spec$ : mat lw_Output_Data$
       pr #Window, fields Layoutwizard_LV1_Spec$ : (mat Layoutwizard_LV1_Captions$, mat Layoutwizard_LV1_Widths, mat Layoutwizard_LV1_Specs$)

       ! Main input loop here
       do
          ! Trim spaces in mat open string
          jndex=0
          for Index=1 to udim(mat OpenString$)
             if len(trim$(OpenString$(Index))) then
                jndex+=1
                openString$(Jndex)=trim$(OpenString$(Index))
             end if
          next Index
          mat OpenString$(jndex+1) : openstring$(Jndex+1)="" ! add 1
          
          curfld(Curfield,Function)
          ! populate it
          pr #Window, fields Layoutwizard_LV1_Pop$ : mat Openstring$
          rinput #Window, fields mat lw_Input_Spec$ : mat lw_Input_Data$(1:1),mat OpenString$,mat lw_Input_Data$(2:4)
          function=fkey
          curfield=curfld
          
          #Select# Function #Case# 99 # 93 # lw_Done
             exitMode=1
          #Case# lw_Browse
             ! Browse for a data file
             open #(FileBrowser:=fngetfilenumber): "name=OPEN:.|*.wb;*.br,recl=1000",external,INPUT IOERR IGNORE
             if file(FileBrowser)>-1 then
                lw_Input_Data$(lw_ProgramName)=file$(FileBrowser)
                close #FileBrowser:
                if lwrc$(lw_input_data$(lw_ProgramName))(1:2)="c:" then
                   lw_input_data$(lw_ProgramName)=lw_input_data$(lw_ProgramName)(3:999)
                   fnScanAll(mat lw_Input_Data$,mat OpenString$)
                end if
             end if
          #Case# lw_ScanProgram
             fnSearchPrograms(mat OpenString$,lw_input_data$(lw_programName),1)
          #Case# lw_ScanOpen
             fnSearchOpenString(mat lw_Input_Data$,mat OpenString$,1)
          #Case# lw_ScanRead
             fnSearchRead(mat lw_Input_Data$,mat OpenString$,1)
          #Case# lw_ScanForm
             fnSearchForm(mat lw_Input_Data$,1)
          #Case# lw_ScanDim
             fnSearchDim(mat lw_Input_Data$,1)
          #Case# lw_ScanAll
             fnScanAll(mat lw_Input_Data$,mat OpenString$)
          #Case# lw_ClearAll
             mat OpenString$(1)=("")
             lw_Input_Data$(lw_ReadString)=""
             lw_Input_Data$(lw_FormString)=""
             lw_Input_Data$(lw_DimString)=""
          #Case# lw_PasteOpen
             mat OpenString$(0)
             sd_dim$=env$("clipboard")
             sd_dim$=srep$(sd_dim$,chr$(13),chr$(10))
             sd_dim$=srep$(sd_dim$,chr$(10)&chr$(10),chr$(10))
             do while pos(sd_dim$,chr$(10))
                mat OpenString$(udim(mat OpenString$)+1)
                openstring$(udim(mat OpenString$))=trim$(sd_dim$(1:pos(sd_dim$,chr$(10))-1))
                sd_dim$(1:pos(sd_dim$,chr$(10)))=""
             loop
             if len(trim$(sd_dim$)) then
                mat OpenString$(udim(mat OpenString$)+1)
                openstring$(udim(mat OpenString$))=trim$(sd_dim$)
                sd_dim$=""
             end if
          #Case# lw_OpenScratchPad
             fnOpenScratchFile
          #Case# lw_Generate
             exitMode=fnGenerateLay(mat OpenString$,lw_Input_Data$(lw_ReadString),lw_Input_Data$(lw_FormString),lw_Input_Data$(lw_DimString),1)
          #End Select#
          
       loop Until ExitMode
       
       close #Window:
    fnend

    def fnScanAll(mat lw_Input_Data$,mat OpenString$)
       if fnIsEmptys(mat OpenString$) then let fnSearchOpenString(mat lw_Input_Data$,mat OpenString$,1)
       fnSearchRead(mat lw_Input_Data$,mat OpenString$,1)
       fnSearchForm(mat lw_Input_Data$,1)
       fnSearchDim(mat lw_Input_Data$,1)
    fnend

    ! 1) Program Name - Search all programs for open statements for the given file name
    ! if found, return program name and open statements
    dim FileList$(1)*255, sp_FileName$(1)*255
    def fnSearchPrograms(mat Openstring$,Folder$*255;___,Index,OpenIndex,FileName$*255,FileIndex,G$)
       if pos(Folder$,"\") then folder$=Folder$(1:pos(Folder$,"\",-1)-1)
       mat sp_FileName$(0)
       for OpenIndex=1 to udim(mat Openstring$)
          fileName$=lwrc$(trim$(fnExtractFileName$(OpenString$(OpenIndex))))
          if len(FileName$) and srch(mat sp_Filename$,FileName$)<=0 then
             fileIndex=udim(mat sp_fileName$)+1
             mat sp_FileName$(FileIndex)
             sp_FileName$(FileIndex)=FileName$
          end if
       next OpenIndex

       if udim(mat sp_FileName$) then

          mat FileList$(0)
          library : fnReadFiles
          fnreadfiles(mat FileList$,trim$(Folder$),"br ",1,0,1)
          fnReadFiles(mat FileList$,trim$(Folder$),"wb ",0,0,1)

          for Index=1 to udim(Mat FileList$)
             if lwrc$(trim$(FileList$(Index)))<>"screenio.br" then
                fnBuildProc("load """&trim$(folder$)&"\"&Trim$(fileList$(Index))&"""")
                g$=">"
                for fileIndex=1 to udim(mat sp_FileName$)
                   fnBuildProc("list 'open' '"&sp_FileName$(fileIndex)&"' "&G$&"temp.[wsid]")
                   g$=">>"
                next fileIndex
                fnBuildProc("run srchhelp")
                fnBuildProc("skip "&str$(udim(mat sp_FileName$)*2+4)&" if code")
                fnProcPrintToFile(trim$(folder$)&"\"&Trim$(fileList$(Index)))
                fnBuildProc("load """&trim$(folder$)&"\"&Trim$(fileList$(Index))&"""")
                for fileIndex=1 to udim(mat sp_FileName$)
                   fnProcListToFile("'open' '"&sp_FileName$(fileIndex)&"'")
                next fileIndex
             end if
          next Index
          fnProcShowFile
          fnRunProc
       end if
    fnend
    
    dim ResultString$*800,so_FileName$*127,so_results$(1)*800,so_files$(1)*127,so_count(1)

    ! 2) Open String - list all "open" and "internal" statements
    def fnSearchOpenString(mat lw_Input_Data$,mat OpenString$;DontShowFile,___,Resultsfile,Index,largest,largeindex,Jndex)
       if len(trim$(lw_input_data$(lw_programName))) then
          fnBuildProc("load """&trim$(lw_input_data$(lw_programName))&"""")
          fnProcListToFile("'open' 'internal'")
          if ~DontShowfile then let fnProcShowFile
          fnClearTempFile
          fnRunProc

          if exists("work.[wsid]") then
             open #(Resultsfile:=fnGetFilenumber): "name=work.[wsid],recl=800",display,input
             mat so_results$(0) : mat so_Files$(0) : mat so_Count(0)
             do until file(ResultsFile)
                linput #ResultsFile: ResultString$ error Ignore
                if file(ResultsFile)=0 then
                   so_Filename$=lwrc$(trim$(fnExtractFileName$(ResultString$)))
                   if len(so_fileName$) then
                      index=udim(mat so_results$)+1
                      mat so_results$(Index)
                      so_results$(Index)=trim$(Resultstring$)
                      index=srch(mat so_files$,so_FileName$)
                      if Index>0 then
                         so_Count(Index)+=1
                      else
                         index=udim(mat so_Files$)+1
                         mat so_Files$(Index) : mat so_Count(Index)
                         so_Files$(Index)=so_Filename$
                         so_Count(Index)+=1
                      end if
                   end if
                end if
             loop
             close #ResultsFile:

             for index=1 to udim(mat so_Count)
                if so_Count(Index)>Largest then
                   largest=so_Count(Index)
                   largeIndex=Index
                end if
             next Index
             
             jndex=0
             for Index=1 to udim(mat so_results$)
                if so_Files$(LargeIndex)=lwrc$(trim$(fnExtractFileName$(so_results$(Index)))) then
                   jndex+=1
                   so_results$(Jndex)=so_results$(Index)
                end if
             next Index
             mat so_results$(Jndex)
             if Jndex then
                mat OpenString$(Jndex)=so_results$
             end if
          end if
       end if
    fnend


    ! 3) Read Statement - Parse Open String, look for file number
    ! or variable name. list all read statements with that file
    ! number or variable name
    dim Var$*255
    def fnSearchRead(mat lw_Input_Data$,mat OpenString$;DontShowFile,___,KeyIndex,AnyThing,ResultsFile,Largest,LargeIndex)
       if len(trim$(lw_input_data$(lw_programName))) then

          for KeyIndex=1 to udim(mat OpenString$)
             ! parse the open statements
             if len(trim$(OpenString$(KeyIndex))) then
                var$=fnExtractOpenVar$(OpenString$(KeyIndex))                        ! Attempt to find open variable
                fnBuildProc("load """&trim$(lw_input_data$(lw_programName))&"""")
                fnProcListToFile("'read' '#"&Var$&"'")
                anyThing=1
             end if
          next KeyIndex
          
          if ~AnyThing then
             ! Scan the given file for all read statements if Open string didn't work
             fnBuildProc("load """&trim$(lw_input_data$(lw_programName))&"""")
             fnProcListToFile("'read'")
          end if

          if ~DontShowFile then let fnProcShowFile
          fnClearTempFile
          fnRunProc

          if exists("work.[wsid]") then
             open #(Resultsfile:=fnGetFilenumber): "name=work.[wsid],recl=800",display,input
             mat so_results$(0)
             do until file(ResultsFile)
                linput #ResultsFile: ResultString$ error Ignore
                if file(ResultsFile)=0 then
                   index=udim(mat so_results$)+1
                   mat so_results$(Index)
                   so_results$(Index)=trim$(Resultstring$)
                   if len(so_results$(Index))>Largest then
                      largest=len(so_results$(Index))
                      largeIndex=Index
                   end if
                end if
             loop
             close #ResultsFile:
             if LargeIndex then lw_Input_Data$(lw_ReadString)=so_results$(LargeIndex)
          end if
       end if
    fnend
    

    ! 4) Form Statement - Parse Read Statement, look for "Using lineref". If lineref is not a variable,
    ! (doesn't contain a "$"), then list that line. If it does contain a dollar sign, then list that
    ! variable to see where it gets set.
    dim LineRef$*800
    def fnSearchForm(mat lw_Input_Data$;DontShowFile,___,NoProc,Index,LargeIndex,Largest,Resultsfile)
       if len(trim$(lw_input_data$(lw_programName))) then
          if len(trim$(lw_Input_data$(lw_readString))) then
             lineref$=fnExtractLineref$(lw_Input_data$(lw_readString))               ! Attempt to find read string
             if LineRef$="" then
                noProc=1
             else if lwrc$(LineRef$(1:5))="""form" then
                lw_Input_Data$(lw_FormString)=trim$(LineRef$,"""")
                noproc=1
             else if pos(LineRef$,"$") then
                fnBuildProc("load """&trim$(lw_input_data$(lw_programName))&"""")
                fnBuildProc("list '"&LineRef$&"' >temp.[wsid]")                      ! Attempt to use line ref
                fnBuildProc("run srchhelp")                                          ! check if it worked
                fnBuildProc("skip printregular if code")                             ! if it failed, jump to regular print
                fnBuildProc("load """&trim$(lw_input_data$(lw_programName))&"""")    ! if it worked, then use it
                fnProcListToFile(LineRef$)
                fnBuildProc("skip finished")                                         ! Skip regular pr if it worked
             else
                fnBuildProc("load """&trim$(lw_input_data$(lw_programName))&"""")
                fnBuildProc("list "&LineRef$&" >temp.[wsid]")                        ! Attempt to use line ref
                fnBuildProc("run srchhelp")                                          ! check if it worked
                fnBuildProc("skip printregular if code")                             ! if it failed, jump to regular print
                fnBuildProc("load """&trim$(lw_input_data$(lw_programName))&"""")    ! if it worked, then use it
                fnProcListToFile(LineRef$,1)
                fnBuildProc("skip finished")                                         ! Skip regular pr if it worked
             end if
          end if
          if ~NoProc then
             fnBuildProc(":printregular")
             fnBuildProc("load """&trim$(lw_input_data$(lw_programName))&"""")
             fnProcListToFile("form")
             fnBuildProc(":finished")
             if ~DontShowFile then let fnProcShowFile
             fnClearTempFile
             fnRunProc

             if exists("work.[wsid]") then
                open #(Resultsfile:=fnGetFilenumber): "name=work.[wsid],recl=800",display,input
                mat so_results$(0)
                do until file(ResultsFile)
                   linput #ResultsFile: ResultString$ error Ignore
                   if file(ResultsFile)=0 then
                      index=udim(mat so_results$)+1
                      mat so_results$(Index)
                      so_results$(Index)=trim$(Resultstring$)
                      if len(so_results$(Index))>Largest then
                         largest=len(so_results$(Index))
                         largeIndex=Index
                      end if
                   end if
                loop
                close #ResultsFile:
                if LargeIndex then lw_Input_Data$(lw_FormString)=so_results$(LargeIndex)
             end if

          end if
       end if
    fnend

    dim sd_Arrays$(1)*127
    dim sd_Dim$*20000

    ! 5) Dim Statement - Parse Read Statement, look for any "Mat" variables. for each one thats found, load
    ! the program, list 'dim' 'arrayname'. If no read statement, list all dims
    def fnSearchDim(mat lw_Input_Data$;DontShowFile,___,tpos,string$*999,sPos,cPos,xPos,Index,ResultsFile)
       if len(trim$(lw_input_data$(lw_programName))) then
          fnBuildProc("load """&trim$(lw_input_data$(lw_programName))&"""")
          if len(trim$(lw_Input_data$(lw_readString))) then
             mat sd_Arrays$(0)
             tPos=0
             string$=lwrc$(trim$(lw_Input_Data$(lw_readString)))
             do while (tPos:=pos(string$,"mat"))
                cPos=pos(String$,",",tpos+4)
                sPos=pos(String$," ",tpos+4)
                if cPos and sPos then
                   xPos=min(cPos,sPos) ! whichever comes first
                else
                   xPos=max(cPos,sPos) ! whichever one there is
                end if
                if ~xPos then xPos=len(String$)+1 ! the rest of the string

                var$=trim$(String$(tpos+3:xPos-1))
                string$=String$(xPos-1:len(string$))
                fnProcListToFile("'dim' '"&Var$&"('")
                index=udim(mat sd_arrays$)+1
                mat sd_Arrays$(Index)
                sd_Arrays$(Index)=Var$
             loop
          else
             fnProcListToFile("dim")
          end if
          if ~DontShowFile then let fnProcShowFile
          fnClearTempFile
          fnRunProc
          
          if exists("work.[wsid]") then
             open #(Resultsfile:=fnGetFilenumber): "name=work.[wsid],recl=800",display,input
             sd_Dim$=""
             do until file(ResultsFile)
                linput #ResultsFile: ResultString$ error Ignore
                if file(ResultsFile)=0 then
                   sd_Dim$=sd_Dim$&lwrc$(trim$(ResultString$))
                end if
             loop
             lw_Input_Data$(lw_DimString)="dim "
             for Index=1 to udim(mat sd_Arrays$)
                lw_Input_Data$(lw_DimString)=lw_Input_Data$(lw_DimString)&sd_Arrays$(Index)&"("&str$(fnReadArraySize(sd_dim$,sd_arrays$(Index)))&"), "
             next Index
             close #ResultsFile:
          end if
       end if
    fnend


    def library Fnreadfiles(Mat Files$,Folder$*255;Extension$,Cleararray,Createfolder,Includeextensions,UseWindowsDIR,___,Dirfile,Dummy$*255,DirMask$*400,Star$)
       ! Note: The UseWindowsDIR parameter uses the windows Dir command instead of BRs in order to read long file names. It will not work under linux.

       star$=fnNeedStar$
       if Cleararray then mat Files$(0)
 !
       if Createfolder then
          if ~Exists(Folder$) then execute Star$&"mkdir "&Folder$
       end if
 !
       if ~Len(Trim$(Extension$)) then extension$="*"

       dirMask$=Folder$&"\*."&Lwrc$(Extension$)
 !
       if UseWindowsDIR then
          fnWindowsReadFiles
       else
          fnStandardReadFiles
       end if
    fnend

    ! These two functions act globally like they're a part of fnReadFiles above.
    def fnWindowsReadFiles(;___,FileName$*255,Ext$*255)
       execute Star$&"system -M dir "&os_filename$(DirMask$)&" >dirlist."&Session$ error SkipReadWindows
       open #(Dirfile:=Fngetfilenumber) : "Name=dirlist.[SESSION], recl=255", display,input
       do
          linput #Dirfile: Dummy$ eof IGNORE
          if File(Dirfile)=0 and Dummy$(3:3)="/" and Dummy$(6:6)="/" and ~Pos(Dummy$,"<DIR>") then
             fileName$=Trim$(Dummy$(40:999))
             ext$=""
             if Pos(FileName$,".")>0 then
                ext$=FileName$(pos(FileName$,".",-1)+1:999)
                if ~IncludeExtensions then
                   fileName$(pos(FileName$,".",-1):999)=""
                end if
             end if

             if Extension$="*" Or Uprc$(Ext$)=Uprc$(Extension$) then
                mat Files$(Udim(Files$)+1)
                files$(Udim(Files$))=FileName$
             end if
          end if
       loop While File(Dirfile)=0
       close #Dirfile:
       SkipReadWindows: !
       execute Star$&"free dirlist.[SESSION]"
    fnend

    ! These two functions act globally like they're a part of fnReadFiles above.
    def fnStandardReadFiles
       execute Star$&"dir "&DirMask$&" >dirlist.[SESSION]" error skipreadStandard
       open #(Dirfile:=Fngetfilenumber) : "Name=dirlist.[SESSION]", display,input
       do
          linput #Dirfile: Dummy$ eof IGNORE
          if File(Dirfile)=0 And ~Pos(Dummy$,"<DIR>") then
             if Extension$="*" Or Uprc$(Dummy$(11:13))=Uprc$(Extension$) then
                if lwrc$(Dummy$(1:13))><"directory of " then
                   if ~(pos(Dummy$," Files, ") and pos(Dummy$," Kilobytes Used, ") and pos(Dummy$," Kilobytes Free")) then
                      mat Files$(Udim(Files$)+1)
                      files$(Udim(Files$))=Lwrc$(Trim$(Dummy$(44:99))(1:32))
                      if Pos(Lwrc$(Files$(Udim(Files$))),".")>0 then
                         files$(Udim(Files$))=Files$(Udim(Files$))(1:Pos(Lwrc$(Files$(Udim(Files$))),".")-1)
                         if Includeextensions then
                            files$(Udim(Files$))=Files$(Udim(Files$))&"."&lwrc$(Dummy$(11:13))
                         end if
                      end if
                   end if
                end if
             end if
          end if
       loop While File(Dirfile)=0
       close #Dirfile:
       skipReadStandard: !
       execute Star$&"free dirlist.[SESSION]"
    fnend

    def fnExtractFileName$*255(&OpenString$)=fnExtractName$("name=",OpenString$)
    def fnExtractKeyFileName$*255(&OpenString$)=fnExtractName$("kfname=",OpenString$)
    def fnExtractName$*255(SearchString$,&OpenString$;___,tpos,ReturnName$*255,spos,SearchStringLen)
       tpos=pos(lwrc$(OpenString$),SearchString$)
       if tpos then
          searchStringLen=len(SearchString$)
          returnName$=OpenString$(tpos+SearchStringLen:pos(lwrc$(OpenString$),",",tpos+SearchStringLen)-1)

          spos=pos(ReturnName$,"/")
          if spos then   ! If linux style, reverse to be Windows style
             returnName$=ReturnName$(spos+1:len(ReturnName$))&"\"&ReturnName$(1:spos-1)
          end if
          fnExtractName$=ReturnName$
       end if
    fnend

    def fnExtractOpenVar$*512(&OpenString$;___,Var$*512,Index,tPos,Return$*512)
       tPos=pos(OpenString$,"#")
       var$=OpenString$(tpos:pos(OpenString$,":",tpos+1))

       ! Skip past any junk characters such as ( and " "
       do until (Index:=Index+1)>len(Var$)
       loop until pos("0123456789abcdefghijklmnopqrstuvwxyz",lwrc$(Var$(Index:Index)))

       ! read the first contiguous string of characters thats either a number or a variable
       do while Index<=len(Var$) and pos("0123456789abcdefghijklmnopqrstuvwxyz",lwrc$(Var$(Index:Index)))
          return$=Return$&Var$(Index:Index)
          index+=1
       loop
       fnExtractOpenVar$=Return$
    fnend
    
    def fnExtractLineref$*800(&ReadString$;___,Var$*800,Index,tPos,Return$*512,bQuotesOn)
       tPos=pos(lwrc$(ReadString$),"using")
       if tpos>0 then
          var$=trim$(ReadString$(tpos+5:len(ReadString$)))

          do while (Index:=Index+1)<=len(Var$)
             if Var$(Index:Index)="""" then bQuotesOn=~bQuotesOn
             return$=Return$&Var$(Index:Index)
          loop until (~bQuotesOn and (Var$(Index:Index)=="," or Var$(Index:Index)==":"))
          if Return$(len(Return$):len(Return$))="," then return$=Return$(1:len(Return$)-1)
          if Return$(len(Return$):len(Return$))=":" then return$=Return$(1:len(Return$)-1)
          fnExtractLineRef$=trim$(Return$)
       end if
    fnend

    ! Proc File Engine
    ! **********************
    dim ProcFileNumber, ProcCompileNumber

    TryNextNumber: ! Try a new number
    compileNumber+=1
    retry

    def fnOpenProc
       open #(ProcFileNumber:=Fngetfilenumber): "name=compile"&str$(CompileNumber)&".$$$, replace", display, output error TryNextNumber
       pr #ProcFileNumber: "proc noecho"
    fnend
    
    def fnBuildSrchHelp(;Nowait)
       if ~exists("srchhelp.br") then
          fnBuildProc("00010 DIM RECORD$*800")
          fnBuildProc("00020 OPEN #1: ""Name= temp.[wsid]"", DISPLAY, INPUT")
          fnBuildProc("00030 LINPUT #1: RECORD$ EOF STOP1")
          fnBuildProc("00040 STOP")
          fnBuildProc("00050 STOP1: STOP 1")
          fnBuildProc("save srchhelp.br")
          fnRunProc(Nowait)
       end if
    fnend

    def library fnBuildProcFile(String$*255)=fnBuildProc(String$)
    def fnBuildProc(String$*255)
       if ~ProcFileNumber then let fnOpenProc
       pr #ProcFileNumber: String$
    fnend

    def library fnRunProcFile(;NoWait)=fnRunProc(Nowait)
    def fnRunProc(;NoWait,___,C$,Star$)
       star$=fnNeedStar$
       if ProcFileNumber then
          pr #ProcFileNumber: "system"
          close #ProcFileNumber:
          procFileNumber=0
          if NoWait then c$=" -C"
          ! execute "type compile"&str$(CompileNumber)&".$$$"
          ! pause
          execute Star$&"system"&C$&" -M "&Fngetbrexe$&" -"&Fngetwbcfg$&" proc compile"&str$(CompileNumber)&".$$$"
       end if
    fnend

    def fnProcPrintToFile(String$*255)
       fnBuildProc("open #1: ""name=filelay\scratchpaper.brs"",display,output")
       fnBuildProc("print #1: """&String$&"""")
       fnBuildProc("close #1: ")
    fnend
    def fnProcListtoFile(String$*255;DontAddQuotes)
       if ~DontAddQuotes then
          if String$(1:1)<>"'" then string$(1:0)="'"
          if String$(len(String$):len(String$))<>"'" then string$=String$&"'"
       end if
       fnBuildProc("list "&String$&" >>filelay\scratchpaper.brs")
       fnBuildProc("list "&String$&" >>work."&wsid$) ! use this WSID, not the procs WSID
    fnend
    def fnClearTempFile
       if exists ("work.[WSID]") then execute fnNeedStar$&"free work.[WSID]"
    fnend
    def fnProcShowFile
       fnBuildProc(fnNeedStar$&"system -C -M start filelay\scratchpaper.brs")
    fnend
    def fnOpenScratchFile
       execute fnNeedStar$&"system -C -M start filelay\scratchpaper.brs"
    fnend

 ! **********************************************************************
 ! Functions To Get Info From Config.Sys Files. By Susan Smith.
 !  Updated By Gabriel Bakker, To Save Time By Running Only Once.
 !
    dim Configfile$*300
    dim Executablefile$*300
 !
    def Fngetwbcfg$*255
       if Configfile$="" then
          fnreadconfigandexe
       end if
       fngetwbcfg$=Configfile$
    fnend
 !
    def Fngetbrexe$*255
       if Executablefile$="" then
          fnreadconfigandexe
       end if
       fngetbrexe$=Executablefile$
    fnend
 !
    dim Configstat$*30000
    def Fnreadconfigandexe(;___,Cr$,Filenumber,Qstart,Qend,Star$)
       star$=fnNeedStar$
       cr$=Hex$("0D0A")
       execute Star$&"status files >TEMPFILE.[session]"
       if Exists("TEMPFILE.[session]") then
          open #(Filenumber:=Fngetfilenumber): "name=TEMPFILE.[session],eol=none",display,input error IGNORE
          if ~File(Filenumber) then
             linput #Filenumber: Configstat$
             close #Filenumber:
 !
             qstart=Pos(Uprc$(Configstat$),"CONFIG FILE")
             if Qstart>0 then
                configfile$=Configstat$(Qstart+13:Qstart+300)
                qend=Pos(Configfile$,Cr$)
                configfile$=Configfile$(1:Qend-1)
                if Configfile$(1:1)=":" then configfile$=Os_Filename$(Configfile$)
             end if
 !
             qstart=Pos(Uprc$(Configstat$),"EXECUTABLE FILE")
             if Qstart>0 then
                executablefile$=Configstat$(Qstart+17:Qstart+300)
                qend=Pos(Executablefile$,Cr$)
                executablefile$=Executablefile$(1:Qend-1)
                if Executablefile$(1:1)=":" then executablefile$=Os_Filename$(Executablefile$)
             end if
          end if
       end if
       if Exists("c:\TEMPFILE.[session]") then execute Star$&"free TEMPFILE.[session] -N"
    fnend


    dim gl_filename$*127,gl_Ver,gl_Pre$, gl_recl, gl_Name$*127
    dim gl_kfname$(1)*127,gl_kpos$(1)*50,gl_klen$(1)*50,gl_kdescription$(1)*255
    dim gl_subs$(1)*127,gl_descr$(1)*127,gl_Form$(1)*50,gl_Extra$(1)*255
    dim gl_TempSubs$(1)*127,gl_TempDescr$(1)*127,gl_TempForm$(1)*50,gl_Mapping(1)
    
    dim gl_kfName$*127
    dim gl_ErrorMessage$*512
    dim gl_TempFileName$*127
    dim FormString$*32000
    def libarary fnGenerateLayout(mat Open_string$,Read_string$*999,Form_string$*20000,Dim_String$*999;DisplayFile)=fnGenerateLay(mat Open_String$,Read_String$,Form_String$,Dim_String$,DisplayFile)
    def fnGenerateLay(mat OpenString$,&ReadString$,&_FormString$,&DimString$;DisplayFile,___,Index,TFile,TPos,KeyIndex,Size,Jndex,FailedRead,FailedForm,FailedMatch,FailedHeader,FailedKey,fPos,Width,kpPos,klPos,kpos,klen)
       formString$=_FormString$
    
       ! Attempt to parse data, or at least get as far as we can
       !  Give error message when we get stuck
       !  Generate the layout with success message when it works
       !  Return ExitMode when success
       
       ! Eventually we can call this code from a function that
       !  searches someones software suite.

       ! Clear values
       mat gl_kfname$(0) : mat gl_kpos$(0)
       mat gl_klen$(0) : mat gl_kDescription$(0)
       gl_FileName$=gl_Pre$=gl_Name$=""
       gl_kfName$=gl_ErrorMessage$=gl_TempFileName$=""
       gl_Ver=gl_recl=0
       
       keyIndex=0

       for OpenIndex=1 to udim(mat OpenString$)
          ! Parse Open String
          if len(Openstring$(OpenIndex)) then
             gl_TempfileName$=fnExtractName$("name=",OpenString$(OpenIndex))
             if ~len(gl_Filename$) then gl_FileName$=gl_TempfileName$

             if len(gl_FileName$) and gl_FileName$=gl_TempFileName$ then
                gl_kfname$=fnExtractName$("kfname=",OpenString$(OpenIndex))

                if len(gl_kfname$) then
                   open #(tFile:=fnGetFileNumber): "name="&gl_filename$&", kfname="&gl_kfname$, internal, input, keyed error Ignore
                   keyIndex=udim(mat gl_kfname$)+1
                   mat gl_kfname$(KeyIndex) : mat gl_kpos$(KeyIndex)
                   mat gl_klen$(KeyIndex) : mat gl_kDescription$(KeyIndex)
                   gl_kfName$(KeyIndex)=gl_kfname$
                else
                   open #(tFile:=fnGetFileNumber): "name="&gl_filename$, internal, input, relative error Ignore
                end if
          
                if file(tFile)=0 then
                   gl_ver=version(tFile)
                   gl_Name$=gl_filename$(pos(gl_filename$,"\",-1)+1:len(gl_filename$))
                   if pos(gl_Name$,".") then
                      gl_Name$=gl_Name$(1:pos(gl_Name$,".")-1)
                   end if
                   gl_Name$=gl_Name$(1:18)
                   gl_Pre$=trim$(gl_Name$)(1:4)&"_"
                   gl_recl=rln(tFile)
                   if kps(tFile)>-1 then
                      gl_kpos$(KeyIndex)=""
                      gl_klen$(KeyIndex)=""
                      index=0
                      do while kps(tFile,(Index:=Index+1))>-1
                         gl_kpos$(KeyIndex)=gl_kpos$(KeyIndex)&str$(kps(tFile,Index))&"/"
                         gl_klen$(KeyIndex)=gl_klen$(KeyIndex)&str$(kln(tFile,Index))&"/"
                      loop
                      gl_kpos$(KeyIndex)=gl_kpos$(KeyIndex)(1:len(gl_kpos$(KeyIndex))-1)
                      gl_klen$(KeyIndex)=gl_klen$(KeyIndex)(1:len(gl_klen$(KeyIndex))-1)
                   end if
                   close #tfile:
                end if
             end if
          end if
       next OpenIndex

       if ~len(gl_filename$) then failedHeader=1
       
       mat gl_TempSubs$(0) : mat gl_TempDescr$(0) : mat gl_TempForm$(0) : mat gl_Mapping(0)
       mat gl_Subs$(0) : mat gl_Descr$(0) : mat gl_Form$(0) : mat gl_Extra$(0)
       if len(ReadString$) and len(FormString$) then
       
          ! Parse Read String
          ! unpack read string
          ! search in form string for each array
          ! use array size from form string to expand each array into one giant list of subscripts
          tPos=pos(ReadString$,":",-1)
          do while len(var$:=fnExtractChunk$(ReadString$,TPos))
             if lwrc$(var$(1:3))="mat" then
                size=fnReadArraySize(DimString$,var$(5:len(var$)))
                if Size then
                   index=udim(mat gl_TempSubs$)
                   mat gl_TempSubs$(Index+Size) : mat gl_TempDescr$(Index+Size)
                   for Jndex=Index+1 to Index+Size
                      if pos(var$,"$") then
                         gl_TempSubs$(Jndex)=srep$(Var$(5:len(var$)),"$","")&str$(Jndex-Index)&"$"
                      else
                         gl_TempSubs$(Jndex)=Var$(5:len(var$))&str$(Jndex-Index)
                      end if
                      gl_TempDescr$(Jndex)=srep$(Var$(5:len(var$)),"$","")&str$(Jndex-Index)
                   next Jndex
                   if pos(var$,"$") then
                      gl_TempSubs$(Index+Size)=srep$(Var$(5:len(var$)),"$","")&"L$"
                   else
                      gl_TempSubs$(Index+Size)=Var$(5:len(var$))&"L"
                   end if

                else
                   FailedRead=1
                end if
             else
                index=udim(mat gl_TempSubs$)+1
                mat gl_TempSubs$(Index) : mat gl_TempDescr$(Index)
                gl_TempSubs$(Index)=Var$
                gl_TempDescr$(Index)=srep$(Var$,"$","")
             end if
          loop until FailedRead
          
          ! Parse Form String
          ! 1) Unpack form string
          fnUnpackFormString(FormString$)
          
          tPos=pos(lwrc$(FormString$),"form ")
          if tPos then tPos+=4 else failedForm=1
          
          fpos=0

          do while len(var$:=fnExtractForm$(FormString$,TPos))
             if lwrc$(var$(1:4))="pos " then
                fpos=val(var$(5:len(Var$)))-1 conv Ignore
             else if lwrc$(var$(1:2))="x " then
                fpos+=val(var$(3:len(Var$))) conv Ignore
             else
                index=udim(mat gl_TempForm$)+1
                mat gl_TempForm$(Index)
                gl_TempForm$(Index)=Var$
             
                ! Find width
                width=fnLengthOf(Var$)
             
                if Width then
                   ! expand Mapping
                   mat gl_Mapping(max(udim(Mat gl_Mapping),fPos+Width))
                   mat gl_Mapping(fpos+1:fPos+Width)=(Index)
                else
                   failedForm=1
                end if
                fpos+=Width
             end if
          loop until FailedForm
          
          index=0
          lastNum=-1
          startPos=0
          
          ! Read Map, find length of consequetive vars
          do while (Index:=Index+1)<=udim(mat gl_Mapping)+1
             if Index>udim(mat gl_Mapping) or LastNum<>gl_Mapping(Index) then
                if LastNum<>-1 then

                   ! Found one, Process it
                   jndex=udim(mat gl_Subs$)+1
                   mat gl_Subs$(Jndex)
                   mat gl_Descr$(Jndex)
                   mat gl_Form$(Jndex)
                   mat gl_Extra$(Jndex)

                   if LastNum then
                      if Index-StartPos=fnLengthOf(gl_TempForm$(LastNum)) then
                         ! Its good, use it
                         if udim(mat gl_TempSubs$)>=LastNum then
                            gl_Subs$(Jndex)=gl_TempSubs$(LastNum)
                            gl_Descr$(Jndex)=gl_TempDescr$(LastNum)
                         else
                            gl_Subs$(Jndex)="_"
                            gl_Descr$(Jndex)=""
                         end if
                         gl_Form$(Jndex)=gl_TempForm$(LastNum)
                      else
                         ! Error. Set it and return error.
                         if udim(mat gl_Tempsubs$)>=LastNum then
                            gl_Subs$(Jndex)=gl_TempSubs$(LastNum)
                            gl_Descr$(Jndex)=gl_TempDescr$(LastNum)
                         else
                            gl_Subs$(Jndex)="_"
                            gl_Descr$(Jndex)=""
                         end if
                         gl_Form$(Jndex)="C "&str$(Index-StartPos)
                         gl_Extra$(Jndex)="Error. Width: "&str$(Index-StartPos)&" but Form: "&gl_TempForm$(LastNum)
                         failedMatch=1
                      end if
                   else
                      ! unknown make it an X
                      gl_Subs$(Jndex)="_"
                      gl_Form$(Jndex)="X "&str$(Index-StartPos)
                      gl_Descr$(Jndex)="Unknown / Unused"
                   end if
                end if
                if Index<=udim(mat gl_Mapping) then lastNum=gl_Mapping(Index)
                startPos=Index
             end if
          loop
       else
          if ~len(ReadString$) then failedRead=1
          if ~len(FormString$) then failedForm=1
       end if

       ! Figure out Keys, mat gl_kpos$,mat gl_klen$,mat gl_kDescription$
       for KeyIndex=1 to udim(mat gl_KDescription$)
          ! Read each chunk of gl_kPos$ and gl_kLen$
          kpPos=KlPos=0
          do while (kpos:=fnExtractKPS(gl_kPos$(KeyIndex),kpPos))
             klen=fnExtractKLN(gl_kLen$(KeyIndex),klPos)
             if kLen then
                ! must do a reverse lookup against mat gl_Mapping
                ! to calculate mat gl_kDescription$

                index=kPos-1
                lastNum=-1

                ! Read Map, find consequetive nums
                do while (Index:=Index+1)<klen+kpos
                   if Index<=udim(mat gl_Mapping) then
                      if LastNum<>gl_Mapping(Index) then
                         ! Found one, Process it
                         if len(gl_Kdescription$(KeyIndex)) then gl_kDescription$(KeyIndex)=gl_kDescription$(KeyIndex)&"/"
                         if gl_Mapping(Index)<=udim(mat gl_TempSubs$) and gl_Mapping(Index)>0 then
                            gl_kDescription$(KeyIndex)=gl_kDescription$(KeyIndex)&srep$(gl_tempSubs$(gl_Mapping(Index)),"$","")
                         else
                            gl_kDescription$(KeyIndex)=gl_kDescription$(KeyIndex)&"Unknown"
                            failedKey=1
                         end if

                         lastNum=gl_Mapping(Index)
                      end if
                   end if
                loop
             else
                if len(gl_Kdescription$(KeyIndex)) then gl_kDescription$(KeyIndex)=gl_kDescription$(KeyIndex)&"/"
                gl_kDescription$(KeyIndex)=gl_kDescription$(KeyIndex)&"Unknown"
                failedKey=1
             end if
          loop
       next KeyIndex

       ! if name is blank then generate a work filename
       if ~len(gl_Name$) then gl_Name$="templayout"&session$
       ! if Recl matches gl_Mapping then autogenerate recl
       if gl_Recl=udim(gl_Mapping) then gl_Recl=0
       
       ! Burn Layout
       fnWriteLay(gl_Name$,gl_filename$,gl_Ver,gl_Pre$,mat gl_kfname$,mat gl_kdescription$,mat gl_subs$,mat gl_descr$,mat gl_Form$,gl_recl,mat gl_Extra$,DisplayFile)

       ! if errors in Header or Detail, pr message box
       gl_ErrorMessage$=""
       if FailedHeader then gl_ErrorMessage$=gl_ErrorMessage$&"We were unable to properly build the Header. Check your open statements."&chr$(13)&chr$(10)
       if FailedKey then gl_ErrorMessage$=gl_ErrorMessage$&"We were unable to figure out the keys. Check your open and read statements."&chr$(13)&chr$(10)
       if FailedRead then gl_ErrorMessage$=gl_ErrorMessage$&"We were unable to figure out the variable names. Check your read statement and dim statement."&chr$(13)&chr$(10)
       if FailedForm then gl_ErrorMessage$=gl_ErrorMessage$&"We were unable to figure out the form specs. Check your form statement."&chr$(13)&chr$(10)
       if FailedMatch then gl_ErrorMessage$=gl_ErrorMessage$&"There was a problem matching your layout specs. Check your Read, Dim and Form statements."&chr$(13)&chr$(10)
       
       ! Return error
       if FailedRead or FailedForm or FailedMatch or FailedHeader or FailedKey then
          msgbox(gl_ErrorMessage$,"I need more information.","Ok")
       else
          fnGenerateLay=1
       end if
    fnend

    def library fnWriteLayout(Name$,filename$*127,Ver,Pre$,mat kfname$,mat kdescription$,mat subs$,mat descr$,mat Form$;Recl,mat Extra$,DisplayFile)=fnWriteLay(Name$,filename$,Ver,Pre$,mat kfname$,mat kdescription$,mat subs$,mat descr$,mat Form$,Recl,mat Extra$)
    def fnWriteLay(Name$,filename$*127,Ver,Pre$,mat kfname$,mat kdescription$,mat subs$,mat descr$,mat Form$;Recl,mat Extra$,___,FileNumber,Index,WidestSub,WidestDescr,WidestForm,Filelayoutpath$*255,FileLayoutExtension$)

       filelayoutpath$=fnSettings$("layoutpath")
       fileLayoutExtension$=fnSettings$("layoutextension")

       library : fnGetFileNumber

       open #(FileNumber:=fnGetFileNumber): "name="&Filelayoutpath$&Name$&FileLayoutExtension$&", recl=1000,replace",display,output
       pr #FileNumber: filename$&", "&Pre$&", "&str$(Ver)
       for Index=1 to udim(mat KfName$)
          pr #FileNumber: KfName$(Index)&", "&KDescription$(Index)
       next Index
       if Recl then
          pr #FileNumber: "recl="&str$(Recl)
       end if
       
       ! Minimums
       widestSub=10
       widestDescr=30
       widestForm=6
       ! Calculate width of output
       for Index=1 to udim(mat Subs$)
          widestSub=max(WidestSub,len(Subs$(Index)))
          widestDescr=max(WidestDescr,len(Descr$(Index)))
          widestForm=max(WidestForm,len(Form$(Index)))
       next Index
       pr #FileNumber: rpt$("=",WidestSub+2+WidestDescr+2+WidestForm)
       for Index=1 to udim(mat Subs$)
          if Index<=udim(mat Extra$) and len(Extra$(Index)) then
             pr #FileNumber: rpad$(Subs$(Index)&", ",WidestSub+2)&rPad$(Descr$(Index)&", ",WidestDescr+2)&lPad$(Form$(Index),WidestForm)&", "&Extra$(Index)
          else
             pr #FileNumber: rpad$(Subs$(Index)&", ",WidestSub+2)&rPad$(Descr$(Index)&", ",WidestDescr+2)&lPad$(Form$(Index),WidestForm)
          end if
       next Index
       close #FileNumber:

       if FileLayoutExtension$="" or FileLayoutExtension$="." then
          if DisplayFile then execute fnNeedStar$&"sy -C -M start notepad "&Filelayoutpath$&name$ ! Have to force notepad because there's no ext to assoc in windows.
       else
          if DisplayFile then execute fnNeedStar$&"sy -C -M start "&Filelayoutpath$&name$&FileLayoutExtension$  ! Just call default program, they have an ext to associate.
       end if
    fnend

    dim fs_Chunk$*800
    dim fs_Replace$*2000
    def fnUnpackFormString(&FormString$;___,pLevel,sPos,ePos,nPos,Index,Num)
       ! unpack the form string
       !  needs to handle:
       !   form C 10,DATE(mm/dd/ccyy),2*(C 10),2*(C 15,C 10),4*(C 15,FMT(##),3*FMT(###))
       !   form C 10,DATE(mm/dd/ccyy),C 10,C 10,C 15,C 10,C 15,C 10,C 15,FMT(##),FMT(###),FMT(###),FMT(###),C 15,FMT(##),FMT(###),FMT(###),FMT(###),C 15,FMT(##),FMT(###),FMT(###),FMT(###),C 15,FMT(##),FMT(###),FMT(###),FMT(###)

       do while (sPos:=pos(FormString$,"*"))    ! find first *
          do while FormString$(Spos+1:Spos+1)=" "
             formString$(Spos+1:Spos+1)=""
          loop
          ! Find Chunk
          if FormString$(sPos:sPos+1)="*(" then ! if next char is "("
             ePos=sPos                      ! find matching ")"
             do                           ! go through string counting
                ePos+=1               ! parens until we're at match
                if FormString$(ePos:ePos)="(" then pLevel+=1
                if FormString$(ePos:ePos)=")" then pLevel-=1
             loop while pLevel
             fs_Chunk$=FormString$(sPos+2:ePos-1)&","
          else ! if its not a "*(" then
             ePos=pos(FormString$,",",sPos) ! look for the next ","
             if ~ePos then ePos=len(FormString$)+1
             fs_Chunk$=FormString$(sPos+1:ePos-1)&","
             ePos-=1
          end if

          ! Find Number
          nPos=sPos
          do while (nPos:=nPos-1)
          loop while pos("0123456789 ",FormString$(nPos:nPos))
          num=val(srep$(FormString$(nPos+1:sPos-1)," ",""))

          ! Then Substitute
          fs_Replace$=rpt$(fs_Chunk$,Num)
          fs_Replace$=fs_Replace$(1:len(fs_Replace$)-1)
          formString$(nPos+1:ePos)=fs_Replace$
       loop        !     go until there's no *
    fnend

    !  needs to handle:
    !   form C 10,DATE(mm/dd/ccyy),2*(C 10),2*(C 15,C 10),4*(C 15,FMT(##),3*FMT(###))
    !   form C 10,DATE(mm/dd/ccyy),C 10,C 10,2*(C 15,C 10),4*(C 15,FMT(##),3*FMT(###))
    !   form C 10,DATE(mm/dd/ccyy),C 10,C 10,C 15,C 10,C 15,C 10,4*(C 15,FMT(##),3*FMT(###))
    !   form C 10,DATE(mm/dd/ccyy),C 10,C 10,C 15,C 10,C 15,C 10,C 15,FMT(##),3*FMT(###),C 15,FMT(##),3*FMT(###),C 15,FMT(##),3*FMT(###),C 15,FMT(##),3*FMT(###)
    !   form C 10,DATE(mm/dd/ccyy),C 10,C 10,C 15,C 10,C 15,C 10,C 15,FMT(##),FMT(###),FMT(###),FMT(###),C 15,FMT(##),FMT(###),FMT(###),FMT(###),C 15,FMT(##),FMT(###),FMT(###),FMT(###),C 15,FMT(##),FMT(###),FMT(###),FMT(###)

    def fnExtractChunk$*255(&ReadString$,&tPos;___,cPos,sPos,xPos)
       do while (ReadString$(tPos+1:tPos+1)=" ")
          tPos+=1
       loop
       cPos=pos(readString$,",",tpos+1)
       sPos=pos(readString$," ",tpos+1)
       if lwrc$(ReadString$(tPos+1:sPos-1))="mat" then
          sPos=pos(readString$," ",sPos+1)
       end if
       if cPos and sPos then
          xPos=min(cPos,sPos)
       else
          xPos=max(cPos,sPos)
       end if
       if ~xPos then xPos=len(ReadString$)+1
       fnExtractChunk$=trim$(ReadString$(tPos+1:xPos-1))
       if ~cPos then cPos=len(ReadString$)+1
       tPos=cPos
    fnend
    
    def fnExtractForm$*255(&FormString$,&tPos;___,xPos)
       xPos=pos(FormString$,",",tpos+1)
       if ~xPos then xPos=len(FormString$)+1
       fnExtractForm$=trim$(FormString$(tPos+1:xPos-1))
       tPos=xPos
    fnend

    def fnExtractKPS(&KPSString$,&tPos;___,xPos,kpos)
       xPos=pos(KPSString$,"/",tpos+1)
       if ~xPos then xPos=len(KPSString$)+1
       kpos=val(KPSString$(tPos+1:xPos-1)) conv Ignore
       fnExtractKPS=kpos
       tPos=xPos
    fnend

    def fnExtractKLN(&KLNString$,&tPos;___,xPos,klen)
       xPos=pos(KLNString$,"/",tpos+1)
       if ~xPos then xPos=len(KLNString$)+1
       klen=val(KLNString$(tPos+1:xPos-1)) conv Ignore
       fnExtractKLN=klen
       tPos=xPos
    fnend

    def fnReadArraySize(&DimString$,Varname$*255;___,tPos,Size$,pPos)
       tPos=pos(lwrc$(DimString$),lwrc$(Varname$))
       if tPos then tPos+=len(VarName$)
       if DimString$(tpos:tpos)="(" then
          pPos=pos(Dimstring$,")",tpos+1)
          if pPos then
             size$=DimString$(tPos+1:pPos-1)
             fnReadArraySize=0
             fnReadArraySize=val(Size$) conv Ignore
          end if
       end if
    fnend

 !
 ! #Autonumber# 19000,10
 FILEIOLIBRARY: ! ********************* File Library ********************
 !
 !
 GETFILENUMBER: ! This Function Finds The Next Empty File Channel
    def library Fngetfilenumber(;Start,Count,___,I)
       if Start=-1 then
          start=fnSettings("startfilenumber")
       end if
       if (Start) then start-=1
       count=max(Count,1)
       do
          if Start>=199 and Start<300 then start=300 : i=0 ! Skip over the invalid 200s
          if File(Start:=Start+1)=-1 then
             i=I+1
          else
             i=0
          end if
       loop until I=Count
       fngetfilenumber=(Start-(Count-1))
    fnend

    def fnResetErrorHandling
       if IgnoreErrors then
          on error goto ReturnFailed
       else
          on error system  ! Turn Error Checking Back Off
       end if
    fnend
 !
 ! #Autonumber# 20000,5
 OPENFILE: ! This Function Opens The Requested File And Returns Filenum
       def library fnOpenFile(&Filename$, Mat F$, Mat F, Mat Form$;Inputonly,Keynum,Dont_Sort_Subs,&Path$,Mat Description$,Mat Fieldwidths,Mat FileIOSubs$,SupressPrompt,IgnoreErrors,CallingProgram$*255,SuppressLog,___,Filenumber,Stringsize,Numbersize,Dummy,Index,Ver,Promptoncreate,Oktocreate,Createlogfile,Form$*2000,StartFileNumber,CountFileNumber,FileLay$*255,CheckIndex,LogLayout$,TempCheckIndex)
          dim Keyname$(1)*255,Kpos$(1)*80,Klen$(1)*80
          dim Fm$*8000,Message$*60
          dim LayoutPath$*255
          fm$=Message$=""

          library : Fngetfilenumber, Fnlog, Fnerrlog
          if IgnoreErrors then on error goto ReturnFailed

          promptOnCreate=Fnsettings("promptonfilecreate")
          enforceDupkeys=Fnsettings("enforcedupkeys")
          startFileNumber=Fnsettings("startfilenumber")

          checkIndex=Fnsettings("checkindex")
          if lwrc$(trim$(FileName$))=lwrc$(trim$(fnSettings$("loglayout"))) then checkIndex=0

          if SuppressLog then
             createLogFile=0
          else
             createlogfile=Fnsettings("createlogfile")
          end if

          if Not (Keynum) then keynum=1
          if Trim$(Path$)<>"" And Path$(Len(Path$):Len(Path$))<>"\" then path$=Path$&"\"
          filelay$=Filename$ ! Make Backup So That We Can Use This Later For Versioning Logic
          if Fnreadfilelayout(Filename$,Recl$,Mat Keyname$,Mat Kpos$,Mat Klen$,Fm$, Stringsize, Numbersize,Dont_Sort_Subs,Ver,Mat Description$,Mat Fieldwidths,Mat FileIOSubs$) then ! Reads Layout And Applies Subscripts
             if fn42 then
                form$=fm$(1:2000)
             else
                form$=trim$(fm$(1:255))
             end if
             if Not Exists(Path$&Filename$) then
                if SupressPrompt then
                   okToCreate=SupressPrompt-1
                else if Promptoncreate then
                   message$=Path$&Filename$
                   if Len(Trim$(Message$))>41 then message$=Message$(1:38)&"..."
                   message$='Should I create "'&Message$&'"?'
                   oktocreate=Fnmessagebox(Message$,"File Not Found")
                else
                   oktocreate=1
                end if
                if Oktocreate then
                   fncreate(Filename$,Recl$,Mat Keyname$,Mat Kpos$,Mat Klen$,Ver,Filelay$,Path$)
                end if
             end if
             if Exists(Path$&Filename$) then
                if Inputonly then
                   do
                      if Udim(Mat Keyname$)>0 and KeyNum>0 then
                         fnCheckIndex(Path$&FileName$,Path$&Keyname$(KeyNum),Kpos$(KeyNum),Klen$(KeyNum),CheckIndex,CreateLogFile,KeyNum,EnforceDupkeys,CallingProgram$)
                         open #(Filenumber:=Fngetfilenumber(StartFileNumber)): "name="&Path$&Filename$&", kfname="&Path$&Keyname$(Keynum)&",shr",internal, input, keyed ioerr ERROR_OPENFILE
                      else
                         open #(Filenumber:=Fngetfilenumber(StartFileNumber)): "name="&Path$&Filename$&",shr",internal, input, relative ioerr ERROR_OPENFILE
                      end if
                   loop While Fnwrongversion(Filenumber,Filelay$,Ver,Path$)
                else
                   do
                      if Udim(Mat Keyname$)>0 and KeyNum>0 then
                         fnCheckIndex(Path$&FileName$,Path$&Keyname$(KeyNum),Kpos$(KeyNum),Klen$(KeyNum),CheckIndex,CreateLogFile,KeyNum,EnforceDupkeys,CallingProgram$)
                         open #(Filenumber:=Fngetfilenumber(StartFileNumber,udim(KeyName$))): "name="&Path$&Filename$&", kfname="&Path$&Keyname$(Keynum)&",shr",internal, outin, keyed ioerr ERROR_OPENFILE
                      else
                         if KeyNum>=-1 then
                            open #(Filenumber:=Fngetfilenumber(StartFileNumber,udim(KeyName$)+1)): "name="&Path$&Filename$&",shr",internal, outin, relative ioerr ERROR_OPENFILE
                         else
                            open #(Filenumber:=Fngetfilenumber(StartFileNumber)): "name="&Path$&Filename$&",shr",internal, outin, relative ioerr ERROR_OPENFILE
                         end if
                      end if
                   loop While Fnwrongversion(Filenumber,Filelay$,Ver,Path$)
                   countFileNumber=FileNumber
                   if KeyNum>=-1 then
                      for Index=1 to Udim(Keyname$) ! Outin Requires All Keys To Be Opened
                         if Index<>Keynum then
                            fnCheckIndex(Path$&FileName$,Path$&Keyname$(Index),Kpos$(Index),Klen$(Index),CheckIndex,CreateLogFile,Index,EnforceDupkeys,CallingProgram$)
                            countFileNumber+=1
                            open #(CountFileNumber): "name="&Path$&Filename$&", kfname="&Path$&Keyname$(Index)&",shr",internal, outin, keyed
                            mat Form$(Max(Udim(Mat Form$),CountFileNumber))
                            form$(CountFileNumber)=Form$
                         end if
                      next Index
                   end if
                end if
                mat Form$(Max(Udim(Mat Form$),Filenumber))
                form$(Filenumber)=Form$
                mat F$(Stringsize)
                mat F(Numbersize)
                fnOpenFile=Filenumber
                logLayout$=fnSettings$("loglayout")
                if Createlogfile and lwrc$(FileLay$)<>lwrc$(LogLayout$) then let Fnlog("Open "&Filename$&" file.",CallingProgram$)
             else
                if Createlogfile then let Fnlog("Error: Failed to open "&Filename$&" file - File Not Found",CallingProgram$,1)
             end if
          else
             pr "Unable to open file layout: "&Filename$
             pr "Make sure it exists, and its not currently in use."
             pause
             stop
          end if
          exitfnOpen: ! Exit Fn Open
       fnend
 ERROR_OPENFILE: ! An Error Was Found Opening The File
       if IgnoreErrors then goto ReturnFailed
       pr "Error parsing file layout or opening file "&filename$&": "&Str$(Err)&" on line: "&Str$(Line)
       pr "A key specified in your file layout could not be found."
       pr "Check your file layout header for errors."
       pause  ! Give User A Chance To Check Variables
       execute "system"

 ReturnFailed: ! Open failed but keep going
    if createlogfile then let fnLog("Error: Failed to open file "&filename$&" or its keys: "&Str$(Err)&" on fileio line: "&Str$(Line),callingprogram$,1)
    goto exitfnOpen

 !
 ! #Autonumber# 21000,10
 CREATE: ! This Should Create The Given File If It Does Not Exist
       def Fncreate(Filename$*255,Recl$,Mat Keys$,Mat Kpos$,Mat Klen$,Ver,Filelay$*255;Path$*255,___,Index,Filenumber)
          if Udim(Mat Keys$)>0 then
             fnCompressIndexParms(KPos$(1),KLen$(1))
             open #(Filenumber:=Fngetfilenumber): "Name="&Path$&Filename$&", "&Recl$&", kfname="&Path$&Keys$(1)&", kps="&Kpos$(1)&", kln="&Klen$(1)&",new",internal,output,keyed
          else
             open #(Filenumber:=Fngetfilenumber): "Name="&Path$&Filename$&", "&Recl$&",new", internal, output, relative
          end if
          version(Filenumber,Ver)
          close #Filenumber:
          for Index=2 to Udim(Mat Keys$)
             fnCompressIndexParms(KPos$(Index),KLen$(Index))
             execute fnNeedStar$&"index "&Path$&Filename$&" "&Path$&Keys$(Index)&" "&Kpos$(Index)&" "&Klen$(Index)&" dupkeys -N"
          next Index
          fncreateversionsaveinfo(Filelay$)
       fnend
 !
 CREATEVERSIONSAVEINFO: ! Backs Up The File Layout And Saves Version Information
       def Fncreateversionsaveinfo(Filelay$*255;Savelay,Newlay,Filename$*255,Prefix$,Dummy$*255,Keyname$,Vers$,Filelayoutpath$*255,FileLayoutExtension$,Oldfilelayout$)
          filelayoutpath$=fnSettings$("layoutpath") ! Retrieve Settings
          fileLayoutExtension$=fnSettings$("layoutextension")

          if ~Exists(Filelayoutpath$&"version") then
             execute fnNeedStar$&"mkdir "&Filelayoutpath$&"version"
          end if
          open #(Newlay:=Fngetfilenumber): "Name="&Filelayoutpath$&Filelay$&FileLayoutExtension$&",recl=511", display,input
          input #Newlay: Filename$,Prefix$,Vers$
          open #(Savelay:=Fngetfilenumber): "Name="&Filelayoutpath$&"version\"&Filelay$&"."&Trim$(Str$(Val((Vers$))))&FileLayoutExtension$&",recl=511, replace", display, output
          pr #Savelay: Fncalculatebackupfilename$(Trim$(Filename$))&", o"&Trim$(Prefix$)&", "&Trim$(Vers$)
          linput #Newlay: Dummy$
          do While Uprc$(Dummy$(1:5))<>"RECL=" and Dummy$(1:4)<>"===="
             pr #Savelay: Fncalculatebackupfilename$(Dummy$(1:Pos(Dummy$,",")-1)) & Trim$(Dummy$(Pos(Dummy$,","):Len(Dummy$)))
             linput #Newlay: Dummy$
          loop
 COPYNEXTLINE: ! Copy The Rest Of The File
          pr #Savelay: Trim$(Dummy$)
          linput #Newlay: Dummy$ eof DONECOPYLINES
          goto COPYNEXTLINE
 DONECOPYLINES: ! File Is Finished
          close #Newlay:
          close #Savelay:
       fnend
 !
 CHECKVERSIONSAVEINFO: ! Ensures Version Save Information Exists
       def Fncheckversionsaveinfo(Filelay$*255,Ver;___,Filelayoutpath$*255,FileLayoutExtension$)
          filelayoutpath$=fnSettings$("layoutpath")
          fileLayoutExtension$=fnSettings$("layoutextension")
          if ~(Trim$(Filelay$(pos(Trim$(Filelay$),".",-1):Len(Trim$(Filelay$))))="."&Trim$(Str$(Ver))) then ! IF this is a version file we're opening, don't create a version save on it.
             if (~Exists(Filelayoutpath$&"version")) Or (~Exists(Filelayoutpath$&"version\"&Filelay$&"."&Trim$(Str$(Ver))&FileLayoutExtension$)) then   ! if its not there, then make it.
                fncreateversionsaveinfo(Filelay$)
             end if
          end if
       fnend
 !
 CALCULATEBACKUPFILENAME: !  Returns The Backup File Name Of The Given File Name
       def Fncalculatebackupfilename$*255(Newfilename$*255;___,Lastslash)
          lastslash=Pos(Newfilename$,"\",-1)
          newfilename$(Lastslash+1:Lastslash)="o"
          fncalculatebackupfilename$=Newfilename$
       fnend
 !
 CALCULATENEWFILENAME: ! Returns The New File Name Of The Given Backup File Name
       def Fncalculatenewfilename$*255(Backupfilename$*255;___,Lastslash)
          lastslash=Pos(Backupfilename$,"\",-1)
          if Backupfilename$(Lastslash+1:Lastslash+1)="o" then
             backupfilename$(Lastslash+1:Lastslash+1)=""
          end if
          fncalculatenewfilename$=Backupfilename$
       fnend
 !
 ! #Autonumber# 22000,2
 CheckIndex: ! Checks to ensure Indexes are up to date
       def fnCheckIndex(MainFile$*255,KeyFile$*255,Kpos$*50,Klen$*50,CheckIndex;Logging,Index,EnforceDupkeys,&CallingProgram$,___,Dupkeys)
          if EnforceDupkeys and Index=1 then dupkeys=0 else dupkeys=1
          if Exists(KeyFile$) then
             if CheckIndex then
                if fnGetFileDT$(KeyFile$)<>fnGetFileDT$(MainFile$) then
                   fnRebuildIndex(MainFile$,Keyfile$,Kpos$,Klen$,1,Logging,Dupkeys,CallingProgram$)
                end if
             end if
          else
             fnRebuildIndex(MainFile$,KeyFile$,Kpos$,Klen$,0,Logging,Dupkeys,CallingProgram$)
          end if
       fnend

 ReIndexAllFiles: ! Rebuilds every index in the system
       def library fnReIndexAllFiles(;___,Index)
          library : fnReIndex, fnReadLayouts
          fnReadLayouts(mat Reindex$)
          for Index=1 to udim(mat Reindex$)
             fnReIndex(Reindex$(Index))
          next Index
       fnend

       dim Reindex$(1)*255
       dim RebuildRecl$*255
       dim RebuildIndexes$(1)*255
       dim RebuildKPos$(1)*255
       dim RebuildKLen$(1)*255
       dim Dummy$(1)*255
       dim Dummy(1)

       ! Included for backwards compatibility
       def library fnRendix(DataFile$*255;CallingProgram$*255) ! .. there was a typeo in an earlier version.
          library : fnReIndex
          fnrendix=fnReIndex(DataFile$,CallingProgram$)
       fnend
       ! Included for compatibility with Susans version, thanks Susan Smith from SMS Software for the Q_INDEX parameter and adding the PATH$ parameter to this function.
       def library fnRebuildKeys(DataFile$*255;Q_INDEX,PATH$*255,CallingProgram$*255)
          library : fnReIndex
          fnRebuildKeys=fnReIndex(DataFile$,CallingProgram$,Q_INDEX,PATH$)
       fnend
 ReIndex: ! Rebuilds all indexes for a given data file
       def library fnReIndex(DataFile$*255;CallingProgram$*255,IndexNum,Path$*255,___,Index,Dummy,Dummy2,Dummy3)
          if CallingProgram$="" then callingProgram$="Unknown"
          fnReadFileLayout(DataFile$,RebuildRecl$,mat RebuildIndexes$,mat RebuildKPos$,mat RebuildKLen$,ReadForm$,Dummy,Dummy2,0,Dummy3,mat dummy$,mat dummy)
          for Index=1 to udim(mat RebuildIndexes$)
             if IndexNum=0 or IndexNum=Index then
                if exists(RebuildIndexes$(Index)) then
                   fnRebuildIndex(Path$&DataFile$,Path$&RebuildIndexes$(Index),RebuildKPos$(Index),RebuildKLen$(Index),1,0,0,CallingProgram$)
                else
                   fnRebuildIndex(Path$&DataFile$,Path$&RebuildIndexes$(Index),RebuildKPos$(Index),RebuildKLen$(Index),0,0,0,CallingProgram$)
                end if
             end if
          next Index
       fnend

 RebuildIndex: ! Rebuilds an Index that was missing or incomplete
       def fnRebuildIndex(MainFile$*255,KeyFile$*255,Kpos$*50,Klen$*50;Update,Logging,Dupkeys,&CallingProgram$)
          fnCompressIndexParms(KPos$,KLen$)

          if Update then
             if Dupkeys then
                execute fnNeedStar$&"index "&MainFile$&" "&KeyFile$&" "&Kpos$&" "&Klen$&" dupkeys replace shr -N" error Ignore
             else
                execute fnNeedStar$&"index "&MainFile$&" "&KeyFile$&" "&Kpos$&" "&Klen$&" replace shr -N" error Ignore
             end if
          else
             if Dupkeys then
                execute fnNeedStar$&"index "&MainFile$&" "&KeyFile$&" "&Kpos$&" "&Klen$&" dupkeys shr -N" error Ignore
             else
                execute fnNeedStar$&"index "&MainFile$&" "&KeyFile$&" "&Kpos$&" "&Klen$&" shr -N" error Ignore
             end if
          end if
          if Logging then let fnLog("FileIO Autocreate Index "&MainFile$&" "&KeyFile$&" "&Kpos$&" "&Klen$,CallingProgram$,1)
       fnend
       
       !
       ! This code tests the following function:
       ! x$="1/3/10" : y$="2/4/3"
       ! fnCompressIndexParms(X$,Y$)
       ! pr "Positions: "&X$
       ! pr "Lengths: "&Y$

       dim IP_Positions$(1),IP_Lens$(1)
       dim IP_Positions(1),IP_Lens(1)
       dim Op_Positions(1),Op_Lens(1)
       
       def fnCompressIndexParms(&Kpos$,&Klen$;___,Index,PreviousSpot,StartCurrent,LengthCurrent,OutDex)
          fnStr2Mat(KPos$,mat IP_Positions$,"/")
          fnStr2Mat(Klen$,mat IP_Lens$,"/")

          mat IP_Positions(udim(mat IP_Positions$))
          mat IP_Lens(udim(mat IP_Lens$))
          
          for Index=1 to udim(mat Ip_Positions)
             iP_Positions(Index)=val(Ip_Positions$(Index)) ! conv ErrorCompressing
             iP_Lens(Index)=val(Ip_Lens$(Index)) ! conv ErrorCompressing
          next Index
          
          mat Op_Positions(0)
          mat Op_Lens(0)
          
          startCurrent=IP_Positions(1)
          lengthCurrent=IP_Lens(1)

          for Index=2 to udim(mat IP_Positions)
             if StartCurrent+LengthCurrent-1<>IP_Positions(Index)-1 then ! If end of previous position is any different then right before current position then
                ! We have a gap, save the entries and reset for the next time before processing this new spot.
                outDex=udim(mat Op_Positions)+1
                mat Op_Positions(OutDex) : mat Op_Lens(OutDex)
                op_Positions(OutDex)=StartCurrent
                op_Lens(OutDex)=LengthCurrent
                
                startCurrent=IP_Positions(Index) ! Record next start position.

                lengthCurrent=0
             end if
             
             lengthCurrent+=IP_Lens(Index) ! Add in the start length
          next Index

          if LengthCurrent then
             outDex=udim(mat Op_Positions)+1
             mat Op_Positions(OutDex) : mat Op_Lens(OutDex)
             op_Positions(OutDex)=StartCurrent
             op_Lens(OutDex)=LengthCurrent
          end if
          
          kpos$=Klen$=""
          
          for Index=1 to udim(mat Op_Positions)
             kPos$=KPos$&str$(Op_Positions(Index))&"/"
             kLen$=KLen$&Str$(Op_Lens(Index))&"/"
          next Index
          
          kPos$=KPos$(1:len(KPos$)-1) ! Strip off final "/"
          kLen$=KLen$(1:len(KLen$)-1) ! Strip off final "/"
       fnend
       
       def fnStr2Mat(String$*10000,mat Matrix$;Delim$,___,Index,Position)
          if Delim$="" then delim$="," ! Default Delimiter
          mat Matrix$(0)
          do while (Position:=pos(String$,Delim$))>0
             index=udim(mat Matrix$)+1
             mat Matrix$(Index)
             matrix$(Index)=String$(1:Position-1)
             string$(1:Position)=""
          loop
          index=udim(mat Matrix$)+1
          mat Matrix$(Index)
          matrix$(Index)=String$
       fnend

 RemoveDeletes: ! ***** Remove Deleted Records - From Susan Smith
       def library fnRemoveDeletes(LayoutName$*255;Path$*255,CallingProgram$*255,DontError,___,OrigFile$*100,TempFile$*100,FileName$*255)
          library : fnReIndex,fnReadLayoutHeader
    !
          a=fnReadLayoutHeader(LayoutName$,FileName$)
          origFile$=FileName$
          tempFile$="RemoveDeletes_"&Session$
    !
          ! If Q_Tempfile$ Exists,
          execute "rename "&Path$&OrigFile$&" "&Path$&TempFile$&" -N" error SkipRemoveDeletes
          execute "copy "&Path$&TempFile$&" "&Path$&OrigFile$&" -D -N"
          if exists(Path$&OrigFile$) then execute fnNeedStar$&"free "&Path$&TempFile$&" -N"
          
          fnReIndex(LayoutName$,CallingProgram$,0,Path$) ! Reindex the file
          fnRemoveDeletes=1
          SkipRemoveDeletes: ! Skip Removing the deletes when the file is in use.
       fnend

 WRONGVERSION: ! Checks Version, Closes And Updates Files If Wrong Version
       def Fnwrongversion(Filenumber,Filelay$*255,Ver;Path$*255,___,Old)
          if (Old:=Version(Filenumber))>=Ver then
             fnwrongversion=0 ! Success
             fncheckversionsaveinfo(Filelay$,Ver)
          else
             close #Filenumber:
             fnupdate(Filelay$,Old,Path$)
             fnwrongversion=1 ! Failure, But Version Was Fixed, So Go Back And Reopen It
          end if
       fnend

 dim UpdateMessages$(1)*80, PrintedYet
 !
 
 dim Dummy$
 ! #Autonumber# 23000,5
 UPDATE: ! Updates File To Latest Version
       def Fnupdate(Filelay$*255,Oldver;&Path$,__,Index,Oldfile,Newfile,Opre$,Npre$,Nndex,Oldfilename$*255,Lastupdatedkey,Errindex,Lastpercent,Reccount,Useraborted,Interval,Numrecs,Oldfilelay$*255,Star$,LayoutPath$*128,FileLayoutExtension$)

          longMessage$="The file layout has been updated. Before proceeding, we must update the data file to the newest version. "
          longMessage$(99999:0)="Choose Yes to continue, or No to Abort.\n\nIf you recently got an update, choose Yes.\n\n"
          longMessage$(99999:0)="If you don't know why you're seeing this message, choose No to Abort and then contact your vendor for support."
          longMessage$=srep$(LongMessage$,"\n",hex$("0d0a"))

          if ~fnSettings("promptonupdate") or (2==msgbox(LongMessage$,"Update","yn","QST")) then ! If don't Prompt, or if they chose "Yes"
             star$=fnNeedStar$
             mat UpdateMessages$(0) : printedYet=0
             layoutpath$=fnSettings$("layoutpath")
             fileLayoutExtension$=fnSettings$("layoutextension")

             on error goto FILEINUSE ! Call Rollback Processing For No Changes Made
             ! open #(Window:=fnGetFileNumber): "SRow=11,SCol=2,Rows=1,Cols=78,Caption=,Border=S",display,outIn
 !
             dim Fm$(1)*1024,Nf$(1)*1024,Nf(1),Of$(1)*1024,Of(1),Nssubs$(1)*80,Ossubs$(1)*80,Nnsubs$(1)*80,Onsubs$(1)*80,Okeys$(1)*80
 !
             library : fnOpenFile, Fnclosefile, Fnreadsubs
 !
             oldfilelay$="version\"&Filelay$&"."&Str$(Oldver)
 !
             if Exists(Layoutpath$&Oldfilelay$&FileLayoutExtension$) then
                fnfindindexes(Oldfilelay$,Oldfilename$,Mat Okeys$)
                fnprint(mat UpdateMessages$,"Making Backup Copy of "&Path$&Fncalculatenewfilename$(Oldfilename$))
                execute Star$&"copy "&Path$&Fncalculatenewfilename$(Oldfilename$)&" "&Path$&Oldfilename$&" -N -D"
                execute Star$&"copy "&Path$&Fncalculatenewfilename$(Oldfilename$)&" "&Path$&Fncalculatenewfilename$(Oldfilename$)&".bak -N"
                execute Star$&"free "&Path$&Fncalculatenewfilename$(Oldfilename$)&" -N"

                on error goto FILEINUSEKEY ! Call Rollback Processing For File Changed And Some Keys

                for Index=1 to Udim(Okeys$)
                   fnprint(mat UpdateMessages$,"Making Backup Copy of "&Path$&Fncalculatenewfilename$(Okeys$(Index)))
                   execute Star$&"copy "&Path$&Fncalculatenewfilename$(Okeys$(Index))&" "&Fncalculatenewfilename$(Okeys$(Index))&".bak -N"
                   execute Star$&"free "&Path$&Fncalculatenewfilename$(Okeys$(Index))&" -N"
                next Index

                on error goto ROLLBACKINFULL ! Call Rollback Processing For File / All Keys Changed

                g_Updating=1
                fnprint(mat UpdateMessages$,"Opening old version of file.")
                oldfile=Fnopen(Oldfilelay$,Mat Of$,Mat Of, Mat Fm$,0,-1,0,Path$,Dummy$,Dummy,0,0,1)
                fnreadsubs(Oldfilelay$,Mat Ossubs$,Mat Onsubs$,Opre$)
                fnprint(mat UpdateMessages$,"Creating new version of file.")
                newfile=Fnopen(Filelay$,Mat Nf$,Mat Nf,Mat Fm$,0,0,0,Path$,Dummy$,Dummy,2,0,1) ! Will Automatically Create
                fnreadsubs(Filelay$,Mat Nssubs$,Mat Nnsubs$,Npre$)
                fnprint(mat UpdateMessages$,"Preforming File Update. SHIFT-A to abort.")
                numrecs=Lrec(Oldfile)

                READOLDRECORD: ! Read The Old Record Here
                read #Oldfile, using Fm$(Oldfile), release: Mat Of$, Mat Of eof DONEUPDATING

                fnUpdateProgressBar((Reccount+=1)/Numrecs,"",.01,0,.15," Updating '"&Filelay$&"' File - Please Wait ","Preforming File Update. Completed: "&Str$(Reccount)&" / "&Str$(Numrecs)&". Press SHIFT-A to abort.")

                mat NF$=("")
                mat NF=(0)

                for Index=1 to Udim(Ossubs$) ! Copy String Values
                   if (Nndex:=Srch(Mat Nssubs$,Ossubs$(Index)))>0 then
                      nF$(Nndex)=trim$(OF$(Index))
                   end if
                next Index
                for Index=1 to Udim(Onsubs$) ! Copy Number Values
                   if (Nndex:=Srch(Mat Nnsubs$,Onsubs$(Index)))>0 then
                      nF(Nndex)=OF(Index)
                   end if
                next Index
                write #Newfile, using Fm$(Newfile) : Mat Nf$, Mat Nf
                if Kstat$="A" then useraborted=1 : goto ROLLBACKINFULL
                goto READOLDRECORD
                DONEUPDATING: ! We Are Finished
                fnclosefile(Oldfile,Oldfilelay$)
                fnclosefile(Newfile,Filelay$)

                close #Window:
                fnCloseProgressBar

                g_Updating=0 ! We Are Done Updating
             else
                pr "Error: Unable to locate old version file layout: "&Layoutpath$&Oldfilelay$&FileLayoutExtension$
                pause
                execute "system"
             end if
             fnResetErrorHandling
          else
             pr "Error: User Aborted"
             pause
             execute "system"
          end if
       fnend
 !
 ! Error Checking Rollback Update Routine.       #Autonumber# 23600,5
 ROLLBACKINFULL: ! Close All Files And Roll Back
       on error system
       updateErrorRecieved=Err
       updateErrorLine=Line
       if Oldfile then let Fnclosefile(Oldfile,Oldfilelay$)
       if Newfile then let Fnclosefile(Newfile,Filelay$)
       lastupdatedkey=Udim(Okeys$) ! Set Value To Roll Back All Indexes
 !
 FILEINUSEKEY: ! Error In Second Part. Partially Roll Back
       on error system
       if UpdateErrorRecieved=0 then updateErrorRecieved=Err
       if UpdateErrorLine=0 then updateErrorLine=Line
       pr Mat UpdateMessages$
       pr "Error encountered or User abort: Rolling back to previous version. Type GO[ENTER] to Quit."
       printedYet=1
       if ~Lastupdatedkey then lastupdatedkey=Index-1
       for Errindex=1 to Lastupdatedkey ! Roll Back All Successful Indexes
          if Exists(Path$&Okeys$(Errindex)) then !:
             execute Star$&"copy "&Path$&Fncalculatenewfilename$(Okeys$(Errindex))&".bak "&Path$&Fncalculatenewfilename$(Okeys$(Errindex)) ! Copy Backup File To Origional File
       next Errindex
       if Exists(Path$&Oldfilename$) then !:
          execute Star$&"copy "&Path$&Fncalculatenewfilename$(Oldfilename$)&".bak "&Path$&Fncalculatenewfilename$(Oldfilename$)
 !
 FILEINUSE: ! Error In First Part
       on error system
       if ~PrintedYet then
          pr Mat UpdateMessages$
          pr "Error encountered or User abort: Rolling back to previous version. Type GO[ENTER] to Quit."
          printedYet=1
       end if
       if UpdateErrorRecieved=0 then updateErrorRecieved=Err
       if UpdateErrorLine=0 then updateErrorLine=Line
       pr Fnerrormessage$(UpdateErrorRecieved,UpdateErrorLine)
       pause
       execute "system"
 !
 ! #Autonumber# 23800,5
 ERRORMESSAGES: ! Return The Proper Error Message For Rollback Errors
       def Fnerrormessage$*80(Errornum,Errorline)
          do
             if Str$(Errorline)(1:2)="21" then
                fnerrormessage$="Error during create: Check heading information in file layout."
                exit do
             end if
             if Errornum=4148 then
                fnerrormessage$="The file was in use. Please close all other copies of BR and try again."
                exit do
             end if
             if Useraborted then
                fnerrormessage$="User Aborted."
                exit do
             end if
             fnerrormessage$="Error updating file: "&Str$(Errornum)&" occured on line: "&Str$(Errorline)
             exit do
          loop
       fnend

       def fnPrint(mat UpdateMessages$,Message$*80;___,Index)
          ! pr #Window, fields "1,1,C 78" : Message$
          fnUpdateProgressBar(0,"",.01,0,.15," Updating '"&Filelay$&"' File - Please Wait ",Message$)

          index=udim(mat UpdateMessages$)+1
          mat UpdateMessages$(Index)
          updateMessages$(Index)=Message$
       fnend

 ! #Autonumber# 24000,2
 FINDINDEXES: ! ***** Reads A File Layout And Finds All Indexes
       def Fnfindindexes(Layoutname$*255;&Filename$,Mat Keys$)
          ! Included for Backwards Compatability
          library : fnReadLayoutHeader
          fnFindIndexes=fnReadLayoutHeader(LayoutName$,FileName$,Mat Keys$)
       fnend

 ReadEntireLayout: ! Reads the entire layout into arrays
       def library fnReadEntireLayout(Layoutname$*255;&Filename$,&Prefix$,Mat Keys$,Mat KeyDescription$,Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos,___,FileNumber)
          library : fnReadLayoutHeader, fnReadLayoutArrays
          filenumber=fnReadLayoutHeader(Layoutname$,Filename$,mat Keys$,mat KeyDescription$,1,Prefix$)
          fnReadEntireLayout=fnReadLayoutArrays(Layoutname$,Prefix$,Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos,FileNumber)
       fnend

 ReadLayoutHeader: ! Reads the layout header and returns Arrays
       def library FnReadLayoutHeader(Layoutname$*255;&Filename$,Mat Keys$,Mat KeyDescription$,LeaveOpen,&Prefix$,___,Count,Keyname$*80,Description$*80,Vers$*80,BKeys,BKeyDescription,Line$*512,LayoutPath$*128,FileLayoutExtension$)
          library : fnGetFileNumber

          layoutName$=trim$(LayoutName$)
          layoutpath$=fnSettings$("layoutpath")
          fileLayoutExtension$=fnSettings$("layoutextension")

          if trim$(layoutname$)<>"" and Exists(Layoutpath$&Layoutname$&FileLayoutExtension$) then
             if Fnsmatrixpresent(Mat Keys$) then bkeys=1
             if FnSMatrixPresent(Mat KeyDescription$) then bKeyDescription=1

             if BKeys then mat Keys$(0)
             if BKeyDescription then mat KeyDescription$(0)

             if ~fnOpenLayout(LayoutPath$&LayoutName$&FileLayoutExtension$) then goto ErrorReadFileLayout

             line$=fnLayoutLinput$
             if uprc$(Line$)<>"#EOF#" then
                fnReadHeading(Line$,Filename$,Prefix$,Vers$)
             else
                pr "Incomplete Layout."
                goto ErrorReadFileLayout
             end if

             line$=fnLayoutLinput$
             if uprc$(Line$)<>"#EOF#" then
                fnReadKeys(Line$,KeyName$,Description$)
             else
                pr "Incomplete Layout."
                goto ErrorReadFileLayout
             end if

             do While Uprc$(Keyname$(1:4))<>"RECL" and KeyName$(1:4)<>"===="
                count+=1
                if BKeys then
                   mat Keys$(Count)
                   keys$(Count)=trim$(Keyname$)
                end if
                if BKeyDescription then
                   Mat KeyDescription$(Count)
                   keyDescription$(Count)=trim$(Description$)
                end if

                line$=fnLayoutLinput$
                if uprc$(Line$)<>"#EOF#" then
                   fnReadKeys(Line$,KeyName$,Description$)
                else
                   pr "Incomplete Layout."
                   goto ErrorReadFileLayout
                end if
             loop
             if Uprc$(KeyName$(1:4))="RECL" Then
                line$=fnLayoutLinput$
                if uprc$(Line$)="#EOF#" then
                   pr "Incomplete Layout."
                   goto ErrorReadFileLayout
                end if
             end if
          end if
          if LeaveOpen then
             fnReadLayoutHeader=1
          else
             fnReadLayoutHeader=Count
          end if
       fnend

 READSUBS: ! Read File Layout, Returning All Subscripts As An Array
       def library Fnreadsubs(Filename$*255,Mat Ssubs$,Mat Nsubs$,&Prefix$)
          ! Included for Backwards Compatability Only
          library : fnReadLayoutArrays
          fnReadLayoutArrays(Filename$,Prefix$,mat SSubs$,mat NSubs$)
       fnend
 !
 READLAYOUTARRAYS: ! Read File Layout, Returning Everything As Some Arrays
       def library Fnreadlayoutarrays(Layoutname$*255,&Prefix$;Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos,AlreadyOpenFileNumber,___,Index,Dummy$*512,Dummy2$*255,Subname$*255,Layout,Description$*255,Spec$*255,Bssubs,Bnsubs,Bsdescription,Bndescription,Bsspec,Bnspec,Bspos,Bnpos,Position,Count,Line$*512)
          library : fnGetFileNumber
 !
          layoutName$=Trim$(LayoutName$)
          position=1
          layoutpath$=fnSettings$("layoutpath") ! Read File Layout Path
          fileLayoutExtension$=fnSettings$("layoutextension")
 !
          if trim$(layoutname$)<>"" and Exists(Layoutpath$&Layoutname$&FileLayoutExtension$) then
             if Fnsmatrixpresent(Mat Ssubs$) then bssubs=1
             if Fnsmatrixpresent(Mat Nsubs$) then bnsubs=1
             if Fnsmatrixpresent(Mat Sspec$) then bsspec=1
             if Fnsmatrixpresent(Mat Nspec$) then bnspec=1
             if Fnsmatrixpresent(Mat Sdescription$) then bsdescription=1
             if Fnsmatrixpresent(Mat Ndescription$) then bndescription=1
             if Fnnmatrixpresent(Mat Spos) then bspos=1
             if Fnnmatrixpresent(Mat Npos) then bnpos=1
 !
             if Bssubs then mat Ssubs$(0)
             if Bnsubs then mat Nsubs$(0)
             if Bsspec then mat Sspec$(0)
             if Bnspec then mat Nspec$(0)
             if Bsdescription then mat Sdescription$(0)
             if Bndescription then mat Ndescription$(0)
             if Bspos then mat Spos(0)
             if Bnpos then mat Npos(0)
 !
             if AlreadyOpenFileNumber then
                ! Then we're already there.
                ! Otherwise, (below) we get the prefix and skip to the details
             else
                if ~fnOpenLayout(LayoutPath$&LayoutName$&FileLayoutExtension$) then goto ErrorReadFileLayout
                line$=fnLayoutLinput$
                if uprc$(Line$)<>"#EOF#" then
                   fnReadHeading(Line$,Dummy$,Prefix$,Dummy2$)
                   prefix$=Trim$(Prefix$)
                else
                   pr "Incomplete Layout."
                   goto ErrorReadFileLayout
                end if
                do
                   line$=fnLayoutLinput$
                   if uprc$(Line$)="#EOF#" then
                      pr "Incomplete Layout."
                      goto ErrorReadFileLayout
                   end if
                loop While Uprc$(Line$(1:4))<>"RECL" and Line$(1:4)<>"===="
                if Uprc$(Line$(1:4))="RECL" Then
                   line$=fnLayoutLinput$
                   if uprc$(Line$)="#EOF#" then
                      pr "Incomplete Layout."
                      goto ErrorReadFileLayout
                   end if
                end if
             end if
 READNEXTLAYOUTLINE: ! Read The Next Line For Processing
             line$=fnLayoutLinput$
             if uprc$(Line$)="#EOF#" then goto DONEREADLAYOUTARRAYS
             subName$=Trim$(Srep$(Line$(1:pos(Line$,",")-1),Chr$(9)," "))
             line$=Line$(pos(Line$,",")+1:len(Line$))
             description$=Trim$(Srep$(Line$(1:pos(Line$,",")-1),Chr$(9)," "))
             line$=Line$(pos(Line$,",")+1:len(Line$))
             if pos(Line$,",") then
                spec$=Trim$(Srep$(Line$(1:pos(Line$,",")-1),Chr$(9)," "))
             else
                spec$=Trim$(Srep$(Line$,Chr$(9)," "))
             end if

             if Trim$(Uprc$(Spec$))(1:2)<>"X " then
                if Trim$(Subname$)<>"" then
                   if Pos(Subname$,"$") then
                      count+=1
                      if Bssubs then
                         mat Ssubs$(Udim(Ssubs$)+1)
                         ssubs$(Udim(Ssubs$))=Trim$(Srep$(Subname$,"$",""))
                      end if
                      if Bsspec then
                         mat Sspec$(Udim(Sspec$)+1)
                         sspec$(Udim(Sspec$))=Trim$(Spec$)
                      end if
                      if Bsdescription then
                         mat Sdescription$(Udim(Sdescription$)+1)
                         sdescription$(Udim(Sdescription$))=Trim$(Description$)
                      end if
                      if Bspos then
                         mat Spos(Udim(Spos)+1)
                         spos(Udim(Spos))=Position
                      end if
                   else
                      if Bnsubs then
                         mat Nsubs$(Udim(Nsubs$)+1)
                         nsubs$(Udim(Nsubs$))=Trim$(Subname$)
                      end if
                      if Bnspec then
                         mat Nspec$(Udim(Nspec$)+1)
                         nspec$(Udim(Nspec$))=Trim$(Spec$)
                      end if
                      if Bndescription then
                         mat Ndescription$(Udim(Ndescription$)+1)
                         ndescription$(Udim(Ndescription$))=Trim$(Description$)
                      end if
                      if Bnpos then
                         mat Npos(Udim(Npos)+1)
                         npos(Udim(Npos))=Position
                      end if
                   end if
                end if
             end if
             position+=Fnlengthof(Trim$(Spec$))
             goto READNEXTLAYOUTLINE
 DONEREADLAYOUTARRAYS: ! We Finished With All The File Layout
             fnReadLayoutArrays=Count
          end if
       fnend

 ReadKeyFiles: ! This function reads the key file list of a file layout
       def library fnReadKeyFiles(layout$,mat keys$;___,filename$*255)
          ! Included for Backwards Compatability
          fnFindIndexes(layout$,filename$,mat keys$)
       fnend

 BuildKey: ! This function builds a key off of mat F$ and mat F
       def library fnBuildKey$*255(layout$*30,mat f$,mat f;keynum,___,filename$*80,prefix$,Index,KeyPart$*30,KeyFields$*80,Key$*255)

          dim BK_Keys$(1)*80
          dim BK_KeyFields$(1)*80
          dim BK_SSubs$(1)*80
          dim BK_NSubs$(1)*80
          dim BK_SSpec$(1)*80
          dim BK_NSpec$(1)*80
          dim BK_LastLayout$*30

          if (trim$(layout$)<>"") and (udim(mat f$)+udim(mat F)) then
             if Layout$<>BK_LastLayout$ then

                library : fnReadEntireLayout, fnKey$

                mat BK_Keys$(0)
                mat BK_KeyFields$(0)

                fnReadEntireLayout(layout$,filename$,prefix$,mat BK_Keys$,mat BK_KeyFields$,mat BK_SSubs$,mat BK_NSubs$,mat BK_SSpec$,mat BK_NSpec$)

                for Index=1 to udim(mat BK_sSubs$)
                   bK_sSubs$(Index)=lwrc$(BK_sSubs$(Index))
                next Index
                for Index=1 to udim(mat BK_nSubs$)
                   bK_nSubs$(Index)=lwrc$(BK_nSubs$(Index))
                next Index

                bK_LastLayout$=Layout$

             end if

             if udim(mat BK_Keys$) then

                if KeyNum=0 then keyNum=1
                keyFields$=lwrc$(trim$(BK_KeyFields$(KeyNum)))

                do
                   if POS(KeyFields$,"/") then
                      keyPart$=KeyFields$(1:pos(KeyFields$,"/")-1)
                      keyFields$(1:pos(KeyFields$,"/"))=""
                   else
                      keyPart$=KeyFields$
                      keyFields$=""
                   end if

                   if (Index:=srch(mat BK_sSubs$,KeyPart$))>0 then
                      key$=Key$&rpad$(rtrm$(F$(Index))(1:fnLengthOf(BK_sSpec$(Index))),fnLengthOf(BK_sSpec$(Index)))
                   else if (Index:=srch(mat BK_nSubs$,KeyPart$))>0 then
                      key$=Key$&cnvrt$(BK_nSpec$(Index),F(Index))
                   end if
                loop while len(KeyFields$)

                fnBuildKey$=Key$
             end if
          end if
       fnend
 !
 ! #Autonumber# 25000,5
 CLOSEFILE: ! ***** Close A File, Closing All Indexes == Use Only With Files Opened I/O With This Library
       def library Fnclosefile(Filenumber, Filelay$*255;Path$*255,Out,___,Remaining,Filename$*255,CloseFileSimple)
       
          dim CF_DummyFileName$*255, CF_DummyFileName2$*255
          ! Out forces not checking if its input or outin - use to increase speed
          !  when you know for a fact the file is opened outin
          !  -1 will force input, 1 will force output, 0 or not specified will check.
          
          closeFileSimple=fnSettings("closefilesimple")
          
          if Out=-1 or (~Out and fnInput(FileNumber)) then
             close #FileNumber:
          else
             remaining=Fnfindindexes(Filelay$,Filename$)
             
             if CloseFileSimple or ~fn418 then
                filename$=srep$(uprc$(filename$),"[WSID]",Wsid$)
                filename$=srep$(uprc$(filename$),"[SESSION]",Session$)
             end if
             
             if kln(FileNumber)=-1 then remaining+=1
             do While Filenumber<=999
                if ~CloseFileSimple and fn418 then
                   sleepRetryCount=0
                   cF_DummyFileName$=uprc$(os_filename$(file$(filenumber))) error SleepRetryCount
                   sleepRetryCount=0
                   cF_DummyFileName2$=uprc$(os_filename$(Path$&filename$)) error SleepRetryCount
                   if CF_DummyFileName$=CF_DummyFileName2$ then
                      close #(Filenumber): ! Close Only If Its This File
                      remaining-=1
                   end if
                else
                   if POS(uprc$(file$(Filenumber)),"\"&uprc$(Filename$)) OR uprc$(file$(Filenumber))=uprc$(Filename$) then
                      close #(Filenumber): ! Close Only If Its This File
                      remaining-=1
                   end if
                end if
                filenumber+=1
             loop While Remaining>0
          end if
       fnend

       def fnInput(FileNumber;___,DisplayFile,Line$*255,Star$)
          star$=fnNeedStar$
          library : fnGetFileNumber
          if exists("fileinfo."&session$) then execute Star$&"free fileinfo."&session$
          execute Star$&"status files >fileinfo."&session$
          open #(DisplayFile:=fnGetFileNumber): "name=fileinfo."&session$, display, input
          ReadNextInfoLine: ! Check the next input line
             linput #DisplayFile: Line$ eof DoneInput
             if pos(lwrc$(Line$),"open file #"&lpad$(str$(FileNumber),3)) then
                linput #DisplayFile: Line$
                if pos(lwrc$(line$),"input") then
                   fnInput=1
                end if
                goto DoneInput
             end if
          goto ReadNextInfoLine
          DoneInput: ! Done fnInput Here
          Close #DisplayFile:
          execute Star$&"free fileinfo."&Session$
       fnend
 !
 ! #Autonumber# 25500,2
 MAKESUBPROC: ! Read File Layout, Returning All Subscripts As A Procfile
       def library Fnmakesubproc(Filename$*255;Mat Subs$,___,Index,Dummy$*512,Dummy2$*255,Subname$*255,Layout,Subs,Prefix$*255,LayoutPath$*255,Stringsize,Numbersize,Line$*512,Star$)
          star$=fnNeedStar$
          layoutpath$=fnSettings$("layoutpath")
          fileLayoutExtension$=fnSettings$("layoutextension")

          if ~fnopenlayout(layoutpath$&filename$&FileLayoutExtension$) then goto ERRORREADFILELAYOUT
          if fnSMatrixPresent(mat Subs$) then
             mat Subs$(0)
          else
             open #(Subs:=Fngetfilenumber): "Name=subs.$$$, replace", display, output
          end if

          line$=fnLayoutLinput$
          if uprc$(Line$)<>"#EOF#" then
             fnReadHeading(Line$,Dummy$,Prefix$,Dummy2$)
             prefix$=Trim$(Prefix$)
          else
             pr "Incomplete Layout."
             goto ErrorReadFileLayout
          end if

          do
             line$=fnLayoutLinput$
             if uprc$(Line$)="#EOF#" then
                pr "Incomplete Layout."
                goto ErrorReadFileLayout
             end if
          loop While Uprc$(Line$(1:4))<>"RECL" and Line$(1:4)<>"===="
          if Uprc$(Line$(1:4))="RECL" Then
             line$=fnLayoutLinput$
             if uprc$(Line$)="#EOF#" then
                pr "Incomplete Layout."
                goto ErrorReadFileLayout
             end if
          end if
 !
 READNEXTSUBPROCLINE: ! Read The Next Line For Processing
          line$=fnLayoutLinput$
          if uprc$(Line$)="#EOF#" then goto DONEMAKESUBPROC
          if pos(Line$,",") then
             subName$=Trim$(Srep$(Line$(1:pos(Line$,",")-1),Chr$(9)," "))
          else
             subName$=Trim$(Srep$(Line$,Chr$(9)," "))
          end if

          if Trim$(Uprc$(Dummy2$))(1:2)<>"X " then
             if Trim$(Subname$)<>"" then
                if Pos(Subname$,"$") then
                   stringsize+=1
                   if Subs then
                      pr #Subs: "let "&Trim$(Prefix$)&Trim$(Srep$(Subname$,"$",""))&"="&Str$(Stringsize)
                   else
                      fnAddToSubsArray("let "&Trim$(Prefix$)&Trim$(Srep$(Subname$,"$",""))&"="&Str$(Stringsize),mat Subs$,Star$)
                   end if
                else
                   numbersize+=1
                   if Subs then
                      pr #Subs: "let "&Trim$(Prefix$)&Trim$(Subname$)&"="&Str$(Numbersize)
                   else
                      fnAddToSubsArray("let "&Trim$(Prefix$)&Trim$(Subname$)&"="&Str$(Numbersize),mat Subs$,Star$)
                   end if
                end if
             end if
          end if
          goto READNEXTSUBPROCLINE
 DONEMAKESUBPROC: ! We Finished With All The File Layout
          if Subs then close #Subs:
       fnend


 dim AddToSubsCount
 AddToSubsArray: ! Adds subscripts to a processing effecient array
       def fnAddToSubsArray(String$*70,mat Subs$,Star$;___,Index)

          addToSubsCount+=1
          index=udim(mat Subs$)

          if Index=0 or AddToSubsCount>31 or (~fn418 and len(Subs$(Index))>127) then
             gosub AddNewOne
          else
             subs$(Index)=Subs$(Index)&":"&String$ SoFlow ErrorAddNewOne
          end if
       fnend

    ErrorAddNewOne: gosub AddNewOne : continue
    AddNewOne: ! Add a new Sub line
       addToSubsCount=1
       index+=1
       mat Subs$(Index)
       subs$(Index)=Star$&String$
    return

    dim Dummy2$(1)*255
    dim Dummy3$(1)*255
    dim Dummy4$(1)*255
    dim Dummy5$(1)*255
    dim ReadForm$*10000
    def library fnReadForm$*10000(Filename$*255;___,Dummy$*255,Dummy,Dummy2,Dummy3)
       fnReadFileLayout(Filename$,Dummy$,mat Dummy$,mat Dummy2$,mat Dummy3$,ReadForm$,Dummy,Dummy2,0,Dummy3,mat Dummy4$,mat Dummy,mat Dummy5$,1)
       fnReadForm$=ReadForm$
    fnend
    def library fnReadFormAndSubs(filename$,mat subs$,&readform$,&stringsize,&numbersize)
       fnReadFileLayout(filename$,dummy$,mat dummy$,mat dummy2$,mat dummy3$,readform$,stringsize,numbersize,0,dummy3,mat dummy4$,mat dummy,mat subs$)
    fnend

    dim LayoutNames$(1)*255
    dim LayoutStart(1)
    
    dim LayoutData$(1)*511
    dim LayoutLine$*511
    
    dim CurrentLayout, LayoutPosition
    
    def library fnClearLayoutCache=fnClearCache
    def fnClearCache
       mat LayoutNames$(0) : mat LayoutStart(0) : mat LayoutData$(0)
    fnend
    
    def fnOpenLayout(LayoutName$*255;___,Layout)
       layoutName$=lwrc$(trim$(LayoutName$))
       currentLayout=srch(mat LayoutNames$,LayoutName$)
       if CurrentLayout>0 then
          ! Found, read it from memory
          fnOpenLayout=1
       else
          library : fnGetFileNumber
          currentLayout=udim(mat LayoutNames$)+1
          mat LayoutNames$(CurrentLayout)
          mat LayoutStart(CurrentLayout)
          
          layoutPosition=udim(mat LayoutData$)+1

          layoutStart(CurrentLayout)=LayoutPosition
          layoutNames$(CurrentLayout)=LayoutName$

          ! Read it from the disk and place it in Memory
          open #(Layout:=Fngetfilenumber): "Name="&LayoutName$&",recl=511,shr", display, input error Ignore

          do while file(Layout)=0
             linput #Layout: LayoutLine$ eof Ignore
             if file(Layout)=0 then
                if len(trim$(LayoutLine$)) and trim$(layoutline$)(1:1)<>"!" then
                   mat LayoutData$(LayoutPosition)
                   layoutData$(LayoutPosition)=LayoutLine$
                   layoutPosition+=1
                   fnOpenLayout=1
                end if
             end if
          loop
          
          mat LayoutData$(LayoutPosition)
          layoutData$(LayoutPosition)="#EOF#"
          close #Layout:
       end if
       layoutPosition=LayoutStart(CurrentLayout)
    fnend
    
    def fnLayoutLinput$*512
       if CurrentLayout and LayoutPosition then
          if LayoutPosition<=udim(mat LayoutData$) then
             fnLayoutLinput$=LayoutData$(LayoutPosition)
             layoutPosition+=1
          end if
       end if
    fnend
    
    dim FormStatement$*10000
 !
 ! #Autonumber# 26000,5
 READFILELAYOUT: ! Read Filelayout, Modify All Parameters, Apply Subscripts
       def Fnreadfilelayout(&Filename$,&Recl$,Mat Keyname$,Mat Kpos$,Mat Klen$,&Form$,&Stringsize,&Numbersize,Dont_Sort_Subs,&Ver,Mat Description$,Mat Fieldwidths;Mat Subs$,DontCForm,___,Position,Prefix$,Laststring,Lastnumber,Subs,Line$*512,CalcRecl,Star$,FileLayoutExtension$)
          dim Rfl_Sdescr$(1)*80,Rfl_Ndescr$(1)*80,Rfl_Descr$(1)*80,Rfl_Swidth(1),Rfl_Nwidth(1),Rfl_Width(1)
          dim StringForm$*4000, NumberForm$*4000
          dim Keyname$*255,Description$*80,Tempform$*80,Subname$*80,Ver$*80

          stringForm$=NumberForm$=""
          keyname$=Description$=Tempform$=Subname$=Ver$=""
          mat Keyname$(0) : mat Kpos$(0) : mat Klen$(0) : mat Rfl_Sdescr$(0) : mat Rfl_Ndescr$(0) : mat Rfl_Descr$(0) : mat Rfl_Swidth(0) : mat Rfl_Nwidth(0) : mat Rfl_Width(0)
          stringsize=0 : numbersize=0 : form$=""

          star$=fnNeedStar$
          layoutpath$=fnSettings$("layoutpath")
          fileLayoutExtension$=fnSettings$("layoutextension")


          if ~fnOpenLayout(LayoutPath$&FileName$&FileLayoutExtension$) then goto ErrorReadFileLayout

          if fnSMatrixPresent(mat Subs$) then
             mat Subs$(0)
          else
             open #(Subs:=Fngetfilenumber): "Name=subs.$$$, replace", display, output
          end if

          line$=fnLayoutLinput$
          if uprc$(Line$)<>"#EOF#" then
             fnReadHeading(Line$,Filename$,Prefix$,Ver$)
             ver=Val(Ver$) error ERRORREADFILELAYOUT
             prefix$=Trim$(Prefix$) : filename$=Trim$(Filename$)
          else
             pr "Incomplete Layout."
             goto ErrorReadFileLayout
          end if

          line$=fnLayoutLinput$
          if uprc$(Line$)<>"#EOF#" then
             fnReadKeys(Line$,KeyName$,Description$)
          else
             pr "Incomplete Layout."
             goto ErrorReadFileLayout
          end if

          do While Uprc$(Keyname$(1:4))<>"RECL" and KeyName$(1:4)<>"===="
             mat Keyname$(Udim(Keyname$)+1) !:
             mat Kpos$(Udim(Keyname$)) !:
             mat Klen$(Udim(Keyname$))
             keyname$(Udim(Keyname$))=Trim$(Keyname$) !:
             kpos$(Udim(Kpos$))=Trim$(Description$) !:
             klen$(Udim(Klen$))=Trim$(Description$)

             line$=fnLayoutLinput$
             if uprc$(Line$)<>"#EOF#" then
                fnReadKeys(Line$,KeyName$,Description$)
             else
                pr "Incomplete Layout."
                goto ErrorReadFileLayout
             end if
          loop
          
          if KeyName$(1:4)="====" then
             calcRecl=1
          else
             recl$=Trim$(Keyname$)
             line$=fnLayoutLinput$
             if uprc$(Line$)="#EOF#" then
                pr "Incomplete Layout."
                goto ErrorReadFileLayout
             end if
          end if
 !
          position=1
 READNEXTLINE: ! Read The Next Line For Processing
          line$=fnLayoutLinput$
          if uprc$(Line$)="#EOF#" then goto DoneReadLayout

          subName$=Trim$(Srep$(Line$(1:pos(Line$,",")-1),Chr$(9)," "))
          line$=Line$(pos(Line$,",")+1:len(Line$))
          description$=Trim$(Srep$(Line$(1:pos(Line$,",")-1),Chr$(9)," "))
          line$=Line$(pos(Line$,",")+1:len(Line$))
          if pos(Line$,",") then
             tempForm$=Trim$(Srep$(Line$(1:pos(Line$,",")-1),Chr$(9)," "))
          else
             tempForm$=Trim$(Srep$(Line$,Chr$(9)," "))
          end if

          fncheckkeys(Mat Kpos$,Mat Klen$,Trim$(Subname$),Position,Trim$(Tempform$)) error ERRORREADFILELAYOUT
          if Trim$(Tempform$)<>"" then
             if Trim$(Uprc$(Tempform$))(1:2)<>"X " then
                mat Rfl_Descr$(Udim(Mat Rfl_Descr$)+1)
                rfl_Descr$(Udim(Mat Rfl_Descr$))=Trim$(Description$)
                mat Rfl_Width(Udim(Mat Rfl_Width)+1)
                rfl_Width(Udim(Mat Rfl_Width))=Fndisplaylengthof(Trim$(Tempform$))
                if Pos(Subname$,"$") then
                   stringsize+=1
                   if Subs then
                      pr #Subs: "let "&Trim$(Prefix$)&Trim$(Srep$(Subname$,"$",""))&"="&Str$(Stringsize)
                   else
                      fnAddToSubsArray("let "&Trim$(Prefix$)&Trim$(Srep$(Subname$,"$",""))&"="&Str$(Stringsize),mat Subs$,Star$)
                   end if
                   if Laststring<>Position And Position<>1 then stringform$=Stringform$&"POS "&Str$(Position)&","
                   fnaddtoform(Stringform$,Trim$(Tempform$))
                   laststring=Position+Fnlengthof(Trim$(Tempform$))
                   mat Rfl_Sdescr$(Stringsize)
                   rfl_Sdescr$(Stringsize)=Trim$(Description$)
                   mat Rfl_Swidth(Stringsize)
                   rfl_Swidth(Stringsize)=Fndisplaylengthof(Trim$(Tempform$))
                else
                   numbersize+=1
                   if Subs then
                      pr #Subs: "let "&Trim$(Prefix$)&Trim$(Subname$)&"="&Str$(Numbersize)
                   else
                      fnAddToSubsArray("let "&Trim$(Prefix$)&Trim$(Subname$)&"="&Str$(Numbersize),mat Subs$,Star$)
                   end if
                   if Lastnumber<>Position then numberform$=Numberform$&"POS "&Str$(Position)&","
                   fnaddtoform(Numberform$,Trim$(Tempform$))
                   lastnumber=Position+Fnlengthof(Trim$(Tempform$))
                   mat Rfl_Ndescr$(Numbersize)
                   rfl_Ndescr$(Numbersize)=Trim$(Description$)
                   mat Rfl_Nwidth(Numbersize)
                   rfl_Nwidth(Numbersize)=Fndisplaylengthof(Trim$(Tempform$))
                end if
             end if
             fnaddtoform(Form$,Trim$(Tempform$))
             position+=Fnlengthof(Trim$(Tempform$))
          end if
          goto READNEXTLINE
 DONEREADLAYOUT: ! We Finished With All The File Layout
          if CalcRecl then recl$="recl="&str$(position-1)
          fnCheckUprc(mat Kpos$,mat Klen$)
          if ~Dont_Sort_Subs then form$=Stringform$&Numberform$
          fnFindAndFixRpt(Form$)
          form$="form "&Form$(1:(Len(Form$)-1))
          if DontCForm then
             ! At least strip out all extra spaces.
             do while pos(Form$,"  ")
                form$=srep$(Form$,"  "," ")
             loop
          else
             formStatement$=Form$ ! Save this for debugging
             form$=Cform$(Form$) error IGNORE
          end if
          if Fnsmatrixpresent(Mat Description$) then
             if Dont_Sort_Subs then ! Resize And Copy Unsorted
                mat Description$(Udim(Mat Rfl_Descr$))=Rfl_Descr$
             else ! Resize And Sort And Copy Seperately
                mat Description$(Stringsize+Numbersize) ! Resize
                if Stringsize>0 then mat Description$(1:Stringsize)=Rfl_Sdescr$ ! Copy Strings
                if Numbersize>0 then mat Description$(Stringsize+1:Stringsize+Numbersize)=Rfl_Ndescr$ ! Copy Numbers
             end if
          end if
          if Fnnmatrixpresent(Mat Fieldwidths) then
             if Dont_Sort_Subs then ! Resize And Copy Unsorted
                mat Fieldwidths(Udim(Mat Rfl_Width))=Rfl_Width
             else ! Resize And Sort And Copy Seperately
                mat Fieldwidths(Stringsize+Numbersize) ! Resize
                if Stringsize>0 then mat Fieldwidths(1:Stringsize)=Rfl_Swidth ! Copy Strings
                if Numbersize>0 then mat Fieldwidths(Stringsize+1:Stringsize+Numbersize)=Rfl_Nwidth ! Copy Numbers
             end if
          end if
          if Subs then close #Subs:
          fnreadfilelayout=1 ! Success!
          ExitfnReadFileLayout: ! Exit fnReadFileLayout
       fnend
 ERRORREADFILELAYOUT: ! Error Encountered Parsing File Layout
       if IgnoreErrors then goto exitfnReadFileLayout
       pr "Error parsing file layout: "&Str$(Err)&" on line: "&Str$(Line)
       if Line=27060 then pr "Look for a missing comma in your detail lines."
       if Line=25110 then pr "Check for proper definition of your keyfiles, terminated by a 'recl=' line."
       if Line=25525 then pr "Subscript too long. Look for a missing comma in your detail lines."
       if Line=56825 then pr "Invalid Form statement: form "&Form$(1:(Len(Form$)-1))
       if Line=25062 then pr "Problem Parsing Version Parameter: "&Ver$
       pause  ! Give User A Chance To Parse Variables
       execute "system"
 !
 ! #Autonumber# 27000,5
 CHECKKEYS: ! Routine To Check If A Given Subname Matches Part Of A Key Spec
       def Fncheckkeys(Mat Kpos$,Mat Klen$,Subname$*50,Position,Spec$*30;___,Index)
          subname$=Srep$(Subname$,"$","")
          for Index=1 to Udim(Mat Kpos$)
             kpos$(Index)=Srep$(uprc$(Kpos$(Index)),uprc$(Subname$),Str$(Position))
             klen$(Index)=Srep$(uprc$(Klen$(Index)),uprc$(Subname$),Str$(Fnlengthof(Spec$)))
          next Index
       fnend
       def fnCheckUprc(Mat Kpos$,mat Klen$;___,Index)
          for Index=1 to Udim(Mat Kpos$)
             kpos$(Index)=Srep$(uprc$(Kpos$(Index)),"-U","")
             klen$(Index)=Srep$(uprc$(Klen$(Index)),"-U","U")
          next Index
       fnend

    def fnReadHeading(Line$*512,&FileName$,&Prefix$,&Ver$)
       fnTakeThing$(Line$,FileName$)
       fnTakeThing$(Line$,Prefix$)
       fnTakeThing$(Line$,Ver$)
    fnend
    
    def fnReadKeys(Line$*512,&KeyName$,&Description$)
       fnTakeThing$(Line$,KeyName$)
       fnTakeThing$(Line$,Description$)
    fnend

    def fnTakeThing$(&Line$,&Thing$)
       if Pos(Line$,",") then
          thing$=Trim$(Srep$(Line$(1:pos(Line$,",")-1),Chr$(9)," "))
          line$=Line$(pos(Line$,",")+1:len(Line$))
       else
          thing$=trim$(srep$(Line$,Chr$(9)," "))
          line$=""
       end if
    fnend
 !
 ! #Autonumber# 27300,10
 LENGTHOF: ! ***** Returns The Length Of A Given Spec
       def library fnLength(Spec$)
          fnLength=FnLengthOf(Spec$)
       fnend
       def Fnlengthof(Spec$*40;_Speclen)
          spec$=Spec$&" " ! This Will Make Sure We At Least Have 1 Space
          if Uprc$(Spec$(1:3))="PIC" then
             _Speclen=Len(trim$(Spec$))-5 ! Pic(##) Returns 2
          else if Uprc$(Spec$(1:3))="FMT" then
             _Speclen=Len(trim$(Spec$))-5 ! Pic(##) Returns 2
          else if Uprc$(Spec$(1:5))="DATE(" then
             _Speclen=Len(trim$(Spec$))-6 ! DATE(mm/dd/ccyy) Returns 10
          else if Uprc$(Spec$(1:2))="S " then
             _Speclen=4
          else if Uprc$(Spec$(1:2))="D " then
             _Speclen=8
          else if Uprc$(Spec$(1:2))="L " then
             _Speclen=9
          else
             _Speclen=Int(Val(Spec$(Pos(Spec$," "):Len(Spec$)))) error ERRORREADFILELAYOUT
          end if
          fnlengthof=_Speclen
       fnend
 !
 ! #Autonumber# 27600,10
 DISPLAYLENGTHOF: ! ***** Returns The Display Length Of A Given Spec
       def library FnDisplayLength(Spec$)
          fnDisplayLength=fndisplaylengthof(Spec$)
       fnend
       def Fndisplaylengthof(Spec$;_Speclen)
          spec$=uprc$(Spec$)
          _Speclen=Fnlengthof(Spec$)
          spec$=Spec$&" "
          if Pos("S PDL D DLDHDTBLBHB N ZDG ",Spec$(1:2)) then ! If Numeric Spec
             if Spec$(1:2)="PD" then ! Pd Is Different
                _Speclen=(2*_Speclen)-1+2
             else if Spec$(1:2)="N " or Spec$(1:2)="ZD" or Spec$(1:2)="G " then
                if pos(Spec$,".") then _Speclen=_Speclen+1 ! Add 2 for the potential "0."
             else ! All The Others Are About The Same
                _Speclen=Len(Str$(256**_Speclen))+2
             end if
          end if
          fndisplaylengthof=_Speclen
       fnend
 !
 ! #Autonumber# 28000,10
 SMATRIXPRESENT: ! ***** Returns True If Optional String Array Is Given
       def Fnsmatrixpresent(;Mat X$)
          if Env$("GUIMODE")="" then ! Non Gui Versions Of Br
             fnsmatrixpresent=Udim(X$) ! Don't support regular test
          else

             on error goto SMATRIXNOTPRESENT
             fnsmatrixpresent=1 ! Assume True
             mat X$(Udim(X$)) ! Redimension Array To Test If Its Real
             goto ENDSMATRIXPRESENT

             SMATRIXNOTPRESENT: ! Error Means Optional Matrix Not Given
             fnsmatrixpresent=0 ! It Wasn't there

             ENDSMATRIXPRESENT: ! We Are Finished
             if G_Updating then
                on error goto ROLLBACKINFULL
             else
                fnResetErrorHandling ! Turn Off Error Checking
             end if
          end if
       fnend
 !
 ! #Autonumber# 28500,10
 NMATRIXPRESENT: ! ***** Returns True If Optional Numeric Array Is Given
       def Fnnmatrixpresent(;Mat X)
          if Env$("GUIMODE")="" then ! Non Gui Versions Of Br
             fnnmatrixpresent=Udim(X) ! Don't support regular test
          else
             on error goto NMATRIXNOTPRESENT
             fnnmatrixpresent=1 ! Assume True
             mat X(Udim(X)) ! Redimension Array To Test If Its Real
             goto ENDNMATRIXPRESENT
 NMATRIXNOTPRESENT: ! Error Means Optional Matrix Not Given
             fnnmatrixpresent=0 ! It Wasn't there
 ENDNMATRIXPRESENT: ! We Are Finished
             if G_Updating then !:
                on error goto ROLLBACKINFULL !:
             else !:
                fnResetErrorHandling ! Turn Off Error Checking
          end if
       fnend
 !
 ! #Autonumber# 29000,10
 ADDTOFORM: ! ***** This Function Adds Spec$ To Form$ Checking For Repetition. Thanks To Mikhail Zheleznov For The Formula.
       def Fnaddtoform(&Form$,Spec$*40;_Lastspec$*40,_Repetition,_Lastchunk$*40)
          _Lastchunk$=Fnlastchunk$(Form$)
          _Lastspec$=Fnextractspec$(_Lastchunk$)
          _Repetition=Fnextractrepetition(_Lastchunk$)
          if Trim$(Uprc$(Spec$))=Trim$(Uprc$(_Lastspec$)) then
             form$=Form$(1:Len(Form$)-1-Len(_Lastchunk$))
             form$=Form$ & Fnbuildchunk$(_Repetition+1,_Lastspec$) & ","
          else
             form$=Form$&Spec$&","
          end if
       fnend
 !
 ! #Autonumber# 29500,10
 BUILDCHUNK: ! ***** Builds A Chunk Of A Form Statment
       def Fnbuildchunk$*40(Repetition,Spec$)
          if Repetition > 1 then
             fnbuildchunk$=Str$(Repetition)&"*"&Spec$
          else
             fnbuildchunk$=Spec$
          end if
       fnend
 !
 ! #Autonumber# 29600,10
 EXTRACTREPETITION: ! ***** Parses A Spec Statement To Get The Rep Count
       def Fnextractrepetition(Spec$*40)
          if Pos(Spec$,"*") then
             fnextractrepetition=Val(Spec$(1:Pos(Spec$,"*")-1))
          else
             fnextractrepetition=1
          end if
       fnend
 !
 ! #Autonumber# 29700,10
 EXTRACTSPEC: ! ***** Parses A Spec Statement To Get The Raw Spec
       def Fnextractspec$*40(Spec$*40)
          if Pos(Spec$,"*") then
             fnextractspec$=Spec$(Pos(Spec$,"*")+1:Len(Spec$))
          else
             fnextractspec$=Spec$
          end if
       fnend
 !
 ! #Autonumber# 29800,10
 LASTCHUNK: ! ***** Extracts The Final Chunk From A Form Statement
       def Fnlastchunk$*40(&Form$) ! Pass By Ref To Save On Workstack
          fnlastchunk$=Form$(Pos(Form$,",",-2)+1:Len(Form$)-1)
       fnend
 !
 FINDANDFIXRPT: ! Searches Your Form Statement For All Subsets Of It That May Be Shortened
       def Fnfindandfixrpt(&Form$;___,Size,Position) ! Pass And Return Byref To Save On Workstack
          dim Chunk$(1)*127
          dim SearchChunk$*4096
          fnparseform(Mat Chunk$,Form$)
          for Size=2 to int(Udim(Mat Chunk$)/2)
             for Position = 1 to Udim(Mat Chunk$)-Size+1
                fnstringtogether$(Mat Chunk$(Position:Position+Size-1),SearchChunk$)
                fnfixrpt(Form$,SearchChunk$)
             next Position
          next Size
       fnend
 !
 PARSEFORM: ! Parse Your Form Splitting Out Chunks Into An Array
       def Fnparseform(Mat Chunk$,&Form$;___,Position,Lastposition)
          mat Chunk$(0)
          do While (Position:=Pos(Form$,",",Position+1))
             mat Chunk$(Udim(Mat Chunk$)+1)
             chunk$(Udim(Mat Chunk$))=Form$(Lastposition+1:Position)
             lastposition:=Position
          loop
       fnend
 !
 STRINGTOGETHER: ! Build String Out Of Array
       def Fnstringtogether$(Mat Chunk$,&String$) ! Pass Byref To Save Workstack
          string$=""
          for Index=1 to Udim(Mat Chunk$)
             string$(4096:0)=Chunk$(Index)
          next Index
       fnend
 !
 FIXRPT: ! Function To Fix Repeating Paterns In Form Statements, Thanks To Luis Gomez And Cls
       def Fnfixrpt(&Tf$,&Strip$;___,Rpt_Time) ! Pass Byref To Save Workstack
          rpt_Time=Int(Len(Tf$)/Len(Strip$))
          do While Pos(Tf$,","&Strip$)>0
             if Rpt_Time>1 then ! Dont put 1*(blahblahblah) because thats stupid
                tf$=Srep$(Tf$,","&Rpt$(Strip$,Rpt_Time),","&Str$(Rpt_Time)&"*("&Strip$(1:len(Strip$)-1)&"),")
             end if
          loop While (Rpt_Time-=1)>1
       fnend
 !
 ! #Autonumber# 34000,10
  UpdateFile: ! Library call to ensure a file is up to date

       def library fnUpdateFile(FileLayout$;___,FileNumber)
          library : fnOpenFile
          dim UpdateForm$(1)*2000
          dim UpdateData$(1)*255
          dim UpdateData(1)

          fileNumber=fnOpen(FileLayout$,mat UpdateData$,mat UpdateData,mat UpdateForm$,1)
          Close #FileNumber:
       fnend

 READDESCRIPTIONS: ! ***** This Function Returns A String Field From A File Given A Key
       def library Fnreaddescription$*255(Fl,Subscript,Key$*255,Mat F$,Mat F,Mat Fm$;DontChangeKey)
          fnReadDescription$=fnReadDes$(Fl,Subscript,Key$,mat f$,mat f,mat fm$,DontChangeKey)
       fnend
       def fnReadDes$*255(Fl,Subscript,Key$*255,mat f$,mat f,mat fm$;DontChangeKey)
          mat F$=("") !:
          mat F=(0)
          if ~DontChangeKey then
             key$=Rpad$(Trim$(Key$)(1:Kln(Fl)),Kln(Fl))
          end if
          read #Fl, using Fm$(Fl), key=key$, release : Mat F$, Mat F nokey IGNORE
          fnReadDes$=trim$(F$(Subscript)(1:255))
       fnend
 READUNOPENEDDESCRIPTION: ! ***** This function returns a string field from a given key and file
       def library fnReadUnopenedDescription$*255(layoutname$,key$*255;Field,DontChangeKey,___,filenumber)
          dim ReadUnopened$(1)*1000, ReadUnopened(1), ReadUnopenedForm$(1)*1000
          library : fnOpenFile
          if ~Field then field=2
          fileNumber=fnOpen(layoutname$,mat ReadUnopened$,mat ReadUnopened,mat ReadUnopenedForm$,1)
          fnReadUnopenedDescription$=fnReadDes$(FileNumber,Field,key$,mat ReadUnopened$,mat ReadUnopened,mat ReadUnopenedForm$,DontChangeKey)
          close #FileNumber:
       fnend

 READNUMBER: ! ***** This Function Returns A Numeric Field From A File Given A Key
       def library Fnreadnumber(Fl,Subscript,Key$*255,Mat F$,Mat F,Mat Fm$;DontChangeKey)
          fnReadNumber=fnReadNum(Fl,Subscript,Key$,Mat f$,mat f,mat fm$,DontChangeKey)
       fnend
 READNUM: ! ***** This Function Returns A Numeric Field From A File Given A Key
       def Fnreadnum(Fl,Subscript,Key$*255,Mat F$,Mat F,Mat Fm$;DontChangeKey)
          mat F$=("") !:
          mat F=(0)
          if ~DontChangeKey then
             key$=Rpad$(Trim$(Key$)(1:Kln(Fl)),Kln(Fl))
          end if
          read #Fl, using Fm$(Fl), key=key$ : Mat F$, Mat F nokey IGNORE
          fnreadnum=F(Subscript)
       fnend
 READUNOPENEDNUMBER: ! ***** This Function Returns A Numeric Field From A File Given A Key
       def library FnreadUnopenedNumber(layoutname$,key$*255;Field,DontChangeKey,___,filenumber)
          library : fnOpenFile
          if ~Field then field=1
          fileNumber=fnOpen(layoutname$,mat ReadUnopened$,mat ReadUnopened,mat ReadUnopenedForm$,1)
          fnReadUnopenedNumber=fnReadNum(FileNumber,Field,key$,mat ReadUnopened$,mat ReadUnopened,mat ReadUnopenedForm$,DontChangeKey)
          close #FileNumber:
       fnend

 READRelative: ! These functions read a relative file by record number and return a single field
       ! ***** This Function Returns A String Field From A File Given A record
       def library Fnreadrelativedescription$*255(Fl,Subscript,RecordNumber,Mat F$,Mat F,Mat Fm$)
          fnReadrelativeDescription$=fnReadrelativeDes$(Fl,Subscript,RecordNumber,mat f$,mat f,mat fm$)
       fnend
       def fnReadrelativeDes$*255(Fl,Subscript,RecordNumber,mat f$,mat f,mat fm$)
          mat F$=("") !:
          mat F=(0)
          read #Fl, using Fm$(Fl), rec=RecordNumber, release : Mat F$, Mat F noRec IGNORE
          fnReadrelativeDes$=F$(Subscript)
       fnend
       ! ***** This function returns a string field from a record and file
       def library fnReadRelUnopenedDescription$*255(layoutname$,RecordNumber;Field,___,filenumber)
          library : fnOpenFile
          if ~Field then field=2
          fileNumber=fnOpen(layoutname$,mat ReadUnopened$,mat ReadUnopened,mat ReadUnopenedForm$,1)
          fnReadRelUnopenedDescription$=fnReadRelativeDes$(FileNumber,Field,RecordNumber,mat ReadUnopened$,mat ReadUnopened,mat ReadUnopenedForm$)
          close #FileNumber:
       fnend

       ! ***** This Function Returns A Numeric Field From A File Given A record
       def library Fnreadrelativenumber(Fl,Subscript,RecordNumber,Mat F$,Mat F,Mat Fm$)
          fnreadrelativenumber=fnReadRelativeNum(Fl,Subscript,RecordNumber,mat F$,mat f,mat fm$)
       fnend

       def fnReadRelativeNum(Fl,Subscript,RecordNumber,Mat F$,Mat F,Mat Fm$)
          mat F$=("") !:
          mat F=(0)
          read #Fl, using Fm$(Fl), rec=RecordNumber : Mat F$, Mat F noRec IGNORE
          fnreadrelativenum=F(Subscript)
       fnend

       def library fnReadRelUnopenedNumber(layoutname$,RecordNumber;Field,___,filenumber)
          library : fnOpenFile
          if ~Field then field=1
          fileNumber=fnOpen(layoutname$,mat ReadUnopened$,mat ReadUnopened,mat ReadUnopenedForm$,1)
          fnReadRelUnopenedNumber=fnReadRelativeNum(FileNumber,Field,RecordNumber,mat ReadUnopened$,mat ReadUnopened,mat ReadUnopenedForm$)
          close #FileNumber:
       fnend

 ReadRecordWhere: ! Search a file for a record based on a filter
       def library fnReadRecordWhere$*255(layoutName$,SearchSub,SearchKey$*255,ReturnSub;___,FileNumber,ReturnValue$*255)
          library : fnOpenFile
          fileNumber=fnOpen(layoutName$,mat ReadUnopened$,mat ReadUnopened,mat ReadUnopenedForm$,1)
          if FileNumber then
             do while file(filenumber)=0
                read #FileNumber, using ReadUnopenedForm$(FileNumber), release : mat ReadUnopened$,mat ReadUnopened error Ignore
                if File(FileNumber)=0 then
                   if lwrc$(trim$(ReadUnopened$(SearchSub)))=lwrc$(trim$(SearchKey$)) then
                      returnValue$=ReadUnopened$(ReturnSub)(1:255)
                   end if
                end if
             loop until len(trim$(ReturnValue$))
             close #FileNumber:
          end if
          fnReadRecordWhere$=ReturnValue$
       fnend

 !
 UNIQUEKEY: ! ***** This Function Searches A File To See If A Key Is Unique
       def library Fnuniquekey(Fl,Key$*255)
          restore #Fl, key=Key$: nokey UNIQUEKEYNOTFOUND
          fnuniquekey=0 ! False - Key Is Found So Its Not Unique
          goto ENDUNIQUEKEY
 UNIQUEKEYNOTFOUND: ! Key Not Found
          fnuniquekey=1 ! True - Key Not Found So It Must Be Unique
 ENDUNIQUEKEY: fnend
 !
 ! #Autonumber# 36000,10
 READALLKEYS: ! ***** Reads A Given String Element (And Optionally A Second Element) From Every Record Of A File Into Given Arrays
       def library Fnreadallkeys(Fl,Mat F$,Mat F,Mat Fm$,Sub1,Mat Out1$;Sub2,Mat Out2$,Index)
          mat Out1$(0)
          if Sub2 then mat Out2$(0)
          restore #Fl:
 NEXTREADALLKEYS: ! Read Next Record
          read #Fl, using Fm$(Fl),release: Mat F$, Mat F eof DONEREADALLKEYS
          mat Out1$(Index:=Udim(Mat Out1$)+1) !:
          out1$(Index)=F$(Sub1)
          if Sub2 then !:
             mat Out2$(Index) !:
             out2$(Index)=F$(Sub2)
          goto NEXTREADALLKEYS
 DONEREADALLKEYS: ! We Finished Reading Here
       fnend
 !
 ! #Autonumber# 36300,10
 READMATCHINGKEYS: ! ***** Reads A Given String Element (And Optionally A Second Element) From Every Record Of A File Into Given Arrays Only When Key Element Matches Specified Key. Key Must Match Specified Element Key.
       def library Fnreadmatchingkeys(Fl,Mat F$,Mat F,Mat Fm$,Key$,Keyfld,Sub1,Mat Out1$;Sub2,Mat Out2$,Index)
          mat Out1$(0)
          if Sub2 then mat Out2$(0)
          restore #Fl, key=Key$: nokey DONEREADMATCHINGKEYS
 NEXTREADMATCHINGKEYS: ! Read Next Record
          read #Fl, using Fm$(Fl),release: Mat F$, Mat F eof IGNORE
          if (File(Fl)=10) Or (F$(Keyfld)<>Key$) then goto DONEREADMATCHINGKEYS
          mat Out1$(Index:=Udim(Mat Out1$)+1) !:
          out1$(Index)=F$(Sub1)
          if Sub2 then !:
             mat Out2$(Index) !:
             out2$(Index)=F$(Sub2)
          goto NEXTREADMATCHINGKEYS
 DONEREADMATCHINGKEYS: ! We Finished Reading Here
       fnend
 !
 ! #Autonumber# 36600,10
 READALLNEWKEYS: ! ***** This Version Of Our Popular Fileio Library Function Reads Only New Items Into The Array.
       def library Fnreadallnewkeys(Fl,Mat F$,Mat F,Mat Fm$,Sub1,Mat Out1$;Dont_Reset,Sub2,Mat Out2$,Index)
          if ~Dont_Reset then mat Out1$(0)
          if (~Dont_Reset) And (Sub2) then mat Out2$(0)
          restore #Fl:
 NEXTREADALLNEWKEYS: ! Read Next Record
          read #Fl, using Fm$(Fl),release: Mat F$, Mat F eof DONEREADALLNEWKEYS
          if Srch(Mat Out1$,F$(Sub1))<1 then ! If Not Found Then
             mat Out1$(Index:=Udim(Mat Out1$)+1) !:
             out1$(Index)=F$(Sub1)
             if Sub2 then !:
                mat Out2$(Index) !:
                out2$(Index)=F$(Sub2)
          end if
          goto NEXTREADALLNEWKEYS
 DONEREADALLNEWKEYS: ! We Finished Reading Here
       fnend
 !
 ! #Autonumber# 37000,10
 READFILTERKEYS: ! ***** Read Matching Keys, But It Also Supports Optional Filters And Up To Four Arrays May Be Populated At Once. If Readlarger=True Then Read All Larger Keys. Function By Mikhail Zheleznov
       def library Fnreadfilterkeys(Fl,Mat F$,Mat F,Mat Fm$,Key$,Keyfld,Sub1,Mat Out1$;Filter$,Filter_Sub,Readlarger,Sub2,Mat Out2$, Sub3, Mat Out3$, Sub4,Mat Out4$, Index)
          mat Out1$(0)
          if Sub2 then mat Out2$(0)
          if Sub3 then mat Out3$(0)
          if Sub4 then mat Out4$(0)
          restore #Fl, key=Key$: nokey DONEREADFILTERKEYS
 NEXTREADFILTERKEYS: ! Read Next Record
          read #Fl, using Fm$(Fl),release: Mat F$, Mat F eof IGNORE
          if (File(Fl)=10) Or (~Readlarger And (F$(Keyfld)<>Key$)) Or (Readlarger And (F$(Keyfld)<Key$)) then goto DONEREADFILTERKEYS
          if (Filter$ ~= "" And F$(Filter_Sub) ~= Filter$) then goto NEXTREADFILTERKEYS
          mat Out1$(Index:=Udim(Mat Out1$)+1) !:
          out1$(Index)=F$(Sub1)
          if Sub2 then !:
             mat Out2$(Index) !:
             out2$(Index)=F$(Sub2)
          if Sub3 then !:
             mat Out3$(Index) !:
             out3$(Index)=F$(Sub3)
          if Sub4 then !:
             mat Out4$(Index) !:
             out4$(Index)=F$(Sub4)
          goto NEXTREADFILTERKEYS
 DONEREADFILTERKEYS: ! We Finished Reading Here
       fnend
 !
 ! #Autonumber# 38000,10
 MAKEUNIQUEKEYS: ! ***** Builds A Unique Key For Given File Of Kylen Length
    def library Fnmakeuniquekey$*255(Fl;Random,Prepend$,___,Index,Size)
       size=kln(fl)-len(Prepend$)
       library : Fnuniquekey
       dim Mukindexs(1)
       mat Mukindexs(Size)=(0)
       if Random then
          randomize
          for Index=1 to udim(MukIndexs)
             mukIndexs(Index)=int(rnd*35)
          next Index
       end if
       do While Not Fnuniquekey(Fl,Prepend$&Fnbuildkeyforunique$(Mat Mukindexs))
          if Random then
             for Index=1 to udim(MukIndexs)
                mukIndexs(Index)=int(rnd*35)
             next Index
          else
             mukindexs(1)+=1
             for Index=1 to Udim(Mukindexs)-1
                if Mukindexs(Index)>35 then mukindexs(Index)=0 : mukindexs(Index+1)+=1
             next Index
             if Mukindexs(Udim(Mukindexs))>35 then let msgbox("The system cannot sustain more then "&Str$(35**Udim(Mukindexs))&" of these. Please delete old ones and try again.")
          end if
       loop Until Mukindexs(Udim(Mukindexs))>35
       fnmakeuniquekey$=Prepend$&Fnbuildkeyforunique$(Mat Mukindexs)
    fnend
 !
 ! #Autonumber# 38500,10
 BUILDKEYFORUNIQUES: ! Builds A Key Out Of The Byte Matrix Given
    def Fnbuildkeyforunique$*255(Mat Bkfuindexs;___,Index,Key$*255)
       for Index=Udim(Bkfuindexs) to 1 step -1
          if bkfuindexs(Index)<10 then
             key$=Key$&str$(bkfuIndexs(Index))
          else
             key$=Key$&Chr$(Bkfuindexs(Index)+55)
          end if
       next Index
       fnbuildkeyforunique$=Key$
    fnend
 !
 ! #Autonumber# 39000,10
 SEARCHCLOSELY: ! ***** Searches Mat A$ Looking For A Partial Match To B$
    def Fnsearchclosely(Mat A$,B$;___,Index,Found)
       for Index=1 to Udim(Mat A$)
          if Pos(A$(Index),B$) then found=Index
       next Index
       fnsearchclosely=Found
    fnend

 dim __Usr$*10000
 !
 ! #Autonumber# 39500,5
 ReadLockedUsers: ! ***** Read all users of the locked file into the given array
    def library fnReadLockedUsers(mat Users$;___,LockFile)
       library : fnGetFileNumber
       execute "Status locks >ERTMP[SESSION]"
       open #(Lockfile:=Fngetfilenumber): "NAME=ERTMP[SESSION]",display,input
       do While __Usr$(1:1)<>"-"
          linput #Lockfile: __Usr$
       loop
       mat Users$(0)
       do until file(LockFile) ! until EOF
          linput #Lockfile: __Usr$ error Ignore
          if file(LockFile)=0 then
             if 1>srch(mat Users$,lwrc$(trim$(__Usr$(26:36)))) then
                mat Users$(udim(mat Users$)+1)
                users$(udim(mat Users$))=trim$(lwrc$(__Usr$(26:36)))
             end if
          end if
       loop
       close #Lockfile:
       execute "free ERTMP[SESSION]"
    fnend
 !
 ! #Autonumber# 40000,10
 ! ****************
 ! **  Log File  **
 ! ****************

    def library fnViewLogFile(;ShowQuit,ShowColumns,ShowExport,___,LogLayout$*255,FileLayoutExtension$,LayoutPath$*255,Index)

       if env$("guimode")="OFF" then execute "config gui on"
       logLayout$=fnSettings$("loglayout")
       layoutPath$=fnSettings$("layoutpath")
       fileLayoutExtension$=fnSettings$("layoutextension")
 
       if len(trim$(loglayout$)) and exists(layoutpath$&loglayout$&FileLayoutExtension$) then
          if lwrc$(trim$(LogLayout$))="logfile" then
             ! Build list of columns to show
             dim Columns$(6),W(6),Descr$(6),Forms$(6)
             columns$(1)="username" : W(1)=10  : Descr$(1)="Username" : Forms$(1)=""
             columns$(2)="program"  : W(2)=12  : Descr$(2)="Program"  : Forms$(2)=""
             columns$(3)="session"  : W(3)=4   : Descr$(3)="#"        : Forms$(3)=""
             columns$(4)="date"     : W(4)=8   : Descr$(4)="Date"     : Forms$(4)="DATE(mm/dd/ccyy)"
             columns$(5)="time"     : W(5)=7   : Descr$(5)="Time"     : Forms$(5)="CC 8"
             columns$(6)="message"  : W(6)=200 : Descr$(6)="Message"  : Forms$(6)=""

             ! Configure filters for fnShowData for the LogFile
             dim FilterFields$(2), filterForm$(2), filterDefaults$(2), filterCompare$(2), filterCaption$(2), filterkey(2)
             filterFields$(1)="date" : filterForm$(1)="DATE(mm/dd/ccyy)" : filterDefaults$(1)=str$(days(date$)) : filterCompare$(1)=">=" : filterCaption$(1)="From Date" : filterKey(1)=1
             filterFields$(2)="date" : filterForm$(2)="DATE(mm/dd/ccyy)" : filterDefaults$(2)=str$(days(date$)) : filterCompare$(2)="<=" : filterCaption$(2)="To Date"   : filterKey(2)=-1

             ! Configure the UI for the LogFile
             dim ShowLogUI$(1)
             mat ShowLogUI$(3+ShowQuit+ShowColumns+ShowExport)

             index=0
             if ShowQuit then showLogUI$(Index+=1)="quitbutton"
             if ShowExport then showLogUI$(Index+=1)="exportbutton"
             if ShowColumns then showLogUI$(Index+=1)="columnsbutton"
             showLogUI$(Index+=1)="search"
             showLogUI$(Index+=1)="border"
             showLogUI$(Index+=1)="caption"

             ! Show the log file using fnShowData
             library : fnShowData
             fnShowData("logfile",0,0,0,20,74,1,"Review Log File","","","",Program$,Dummy,mat Columns$,mat ShowLogUI$,mat Descr$,mat W,mat Forms$,"date",mat FilterFields$,mat FilterForm$,mat FilterCompare$,mat FilterCaption$,mat FilterDefaults$,mat FilterKey)
          else
             fnShowData(LogLayout$,0,0,0,20,74,0,"Review Log File","","","",Program$)
          end if
       else if exists("fileio.log") then
          execute "system -C -M start notepad fileio.log"
       end if
    fnend

 ERRLOG: ! ***** Log An Error Entry
    def library Fnerrlog(String$*255;CallingProgram$*255,___,Filehandle)
       library : fnLog
       fnLog("Error("&Str$(Err)&":"&Str$(Line)&") "&String$,CallingProgram$)
    fnend

    dim LogChanges$(1)*1024, LogChanges(1)
    dim LogSSubs$(1)*255,LogNSubs$(1)*255
    def library fnSetLogChanges(mat f$,mat F)
       mat LogChanges$(udim(mat F$))=F$
       mat LogChanges(udim(mat F))=F
    fnend
    def library fnLogChanges(mat f$,mat F;String$*1048,CallingProgram$*255,Layout$,___,Index,D$)
       library : fnLog, fnReadSubs
       if len(Layout$) then
          fnReadSubs(Layout$,mat LogSSubs$,mat LogNSubs$,D$)
       else
          mat LogSSubs$(udim(mat F$)) : mat LogNSubs$(udim(mat F))
          for Index=1 to udim(mat F$)
             logSSubs$(Index)="String "&str$(Index)
          next Index
          for Index=1 to udim(mat F)
             logNSubs$(Index)="Number "&str$(Index)
          next Index
       end if

       string$=String$&" Changed: "
       for Index=1 to min(udim(mat F$),udim(mat LogChanges$))
          if trim$(F$(Index))<>trim$(LogChanges$(Index)) then
             if len(String$&" "&LogSSubs$(Index)&":"""&trim$(LogChanges$(Index))&""" to """&trim$(f$(Index))&""".")<512 then
                string$=String$&" "&LogSSubs$(Index)&":"""&trim$(LogChanges$(Index))&""" to """&trim$(f$(Index))&"""."
             else if len(String$&" skipped -")<512 then
                string$=String$&" skipped -"
             end if
          end if
       next Index
       for Index=1 to min(udim(mat F),udim(mat LogChanges))
          if F(Index)<>LogChanges(Index) then
             if len(String$&" "&LogNSubs$(Index)&":"""&str$(LogChanges(Index))&""" to """&str$(f(Index))&""".") < 512 then
                string$=String$&" "&LogNSubs$(Index)&":"""&str$(LogChanges(Index))&""" to """&str$(f(Index))&"""."
             end if
          end if
       next Index
       
       fnLog(String$(1:512),CallingProgram$)
    fnend

 LogArray: ! ***** Log an array and a message
    def library fnLogArray(mat F$,mat F;String$*512,CallingProgram$*255,___,Length,Index,FileNumber)
       library : fnlog

       for Index=1 to udim(mat F$)
          if len(String$)+len(f$(Index))+2>512 then
             fnLog(String$,CallingProgram$)
             string$=""
          end if
          string$=String$&trim$(F$(Index)(1:500))&", "
       next Index
       for Index=1 to udim(mat F)
          if len(String$)+len(str$(f(Index)))+2>512 then
             fnLog(String$,CallingProgram$)
             string$=""
          end if
          string$=String$&str$(F(Index))(1:500)&", "
       next Index
       fnLog(String$,CallingProgram$)
    fnend

    dim LogData$(1)*512
    dim LogData(1)
    dim LogForm$(1)*256

 LOG: ! ***** Log An Entry
    def library Fnlog(String$*512;CallingProgram$*255,ForceTextfile,___,Filehandle,LogLibrary$*255,LogLayout$)
       logLibrary$=fnSettings$("loglibrary")
       logLayout$=fnSettings$("loglayout")

       if ~ForceTextfile and len(trim$(LogLibrary$)) then
          library LogLibrary$ : fnFileIOLog
          if len(trim$(LogLayout$)) then
             fnLog=fnFileIOLog(String$,Login_Name$,Session$,days(Date$("mm/dd/ccyy"),"mm/dd/ccyy"),Time$("hh:mm:ss"),CallingProgram$(len(CallingProgram$)-39:len(CallingProgram$)))
          else
             if len(CallingProgram$) then callingProgram$=CallingProgram$&" - "
             fnLog=fnFileIOLog(CallingProgram$(len(CallingProgram$)-39:len(CallingProgram$))&String$)
          end if
       else if ~ForceTextfile and len(trim$(LogLayout$)) then
          library : fnOpenFile,fnCloseFile
          fileHandle=fnOpen(LogLayout$,mat LogData$,mat LogData,mat LogForm$,0,0,0,"",mat Dummy$,mat Dummy,0,1)
          if FileHandle then
             logData$(log_Program)=CallingProgram$(len(CallingProgram$)-39:len(CallingProgram$))
             logData$(log_Username)=Login_Name$
             logData$(log_Session)=Session$
             logData$(log_Time)=Time$("hh:mm:ss")
             logData$(log_Message)=String$(1:255)
             logData(log_date)=days(Date$("mm/dd/ccyy"),"mm/dd/ccyy")

             write #FileHandle, using LogForm$(FileHandle) : mat LogData$,mat LogData error ErrorUseTextFile
             fnCloseFile(FileHandle,LogLayout$,"",1)
          else
             goto ErrorUseTextFile
          end if
       else
          UseTextFile: ! Use a text logfile.
          loggingRetryCount=0
          library : Fngetfilenumber
          open #(Filehandle:=Fngetfilenumber) : "Name=FileIO.log, use, recl=658",display,output locked RetryLogging
          if file(filehandle)<>-1 then
             pr #Filehandle: "User: "&Login_Name$&" - Session ID: "&Session$&" - Date: "&Date$("mm/dd/ccyy")&" - "&Time$("hh:mm:ss")&" - Program: "&CallingProgram$&": "&String$
             close #Filehandle:
          end if
       end if
    fnend

 ErrorUseTextFile: ! Couldn't write to log file, msg user and use text
    msgbox("A file is locked on the server. Please restart the server if the problem continues.")
 goto UseTextFile

 dim LoggingRetryCount
 
 RetryLogging: !
    if LoggingRetryCount<20 then
       loggingRetryCount+=1
       sleep(.1)
       retry
    else
       loggingRetryCount=0
       continue
    end if

 !
 ! #Autonumber# 42000,10
 NOTINFILE: ! ***** Search A Given Spot In A File For A Given String, Return True If Not Found
    def library Fnnotinfile(String$*100,Filename$,Sub;Path$,Filenumber)
       library : fnOpenFile,Fnreadallkeys
       dim F$(1)*255,F(1),Fmmm$(1)*2000,Filelist$(1)*255
       filenumber=Fnopen(Filename$,Mat F$,Mat F,Mat Fmmm$,1,0,0,Path$)
       fnreadallkeys(Filenumber,Mat F$,Mat F,Mat Fmmm$,Sub,Mat Filelist$)
       if Fnsearchclosely(Mat Filelist$,String$)=0 then let Fnnotinfile=1
       close #Filenumber: !:
    fnend

 GetFileDateTime: ! Performs a directory listing to get the date of a file
    def library fnGetFileDateTime$(FileName$*255)=fnGetFileDT$(FileName$)
    def fnGetFileDT$(FileName$*255;___,DirFile,Dummy$*255,Star$)
       star$=fnNeedStar$
       library : Fngetfilenumber
       execute Star$&"dir "&FileName$&" >xxx[SESSION]"
       open #(Dirfile:=Fngetfilenumber) : "Name=xxx[SESSION]", display,input
       do
          linput #Dirfile: Dummy$ eof Ignore
          if file(DirFile)=0 then
             if lwrc$(trim$(Dummy$(46:99)))=lwrc$(trim$(FileName$(pos(FileName$,"\",-1)+1:len(FileName$)))) then
                fnGetFileDT$=Dummy$(27:42)
             end if
          end if
       loop while file(DirFile)=0
       close #Dirfile:
       errorRetrycount=0
       execute Star$&"free xxx[SESSION] -n" error RetryCount
    fnend
 !
 ! #Autonumber# 43000,10
 READLAYOUTS: ! ***** Reads Into A Given Array The List Of File Layouts
    def library Fnreadlayouts(Mat Dirlist$;OverrideExtension$,___,Dummy$*255,Dirfile,FileLayoutExtension$,Layoutpath$*255,Star$)
       star$=fnNeedStar$
       library : Fngetfilenumber

       layoutpath$=fnSettings$("layoutpath")
       fileLayoutExtension$=fnSettings$("layoutextension")
       if OverrideExtension$><"" then fileLayoutExtension$=OverrideExtension$

       mat Dirlist$(0)
       execute star$&"dir "&layoutPath$&"*"&FileLayoutExtension$&" >xxx" error DoneReadingFileNames
       open #(Dirfile:=Fngetfilenumber) : "Name=xxx", display,input
       READFILENAME: ! Read The Next File Name
       linput #Dirfile: Dummy$ eof DONEREADINGFILENAMES
       if trim$(uprc$(dummy$(11:13)))=trim$(uprc$(FileLayoutExtension$(2:4))) and ~pos(dummy$,"<DIR>") then
          mat dirlist$(udim(dirlist$)+1)
          dummy$=trim$(dummy$(44:99))
          if FileLayoutExtension$<>'' and FileLayoutExtension$><"." then
             dummy$=dummy$(1:pos(lwrc$(dummy$),lwrc$(FileLayoutExtension$))-1)
          end if
          dirlist$(udim(dirlist$))=dummy$(1:30)
       end if
       goto READFILENAME
       DONEREADINGFILENAMES: ! Finished
       if DirFile and file(DirFile)>-1 then close #Dirfile:
       execute Star$&"free xxx"
    fnend


    def library fnDirVersionHistoryFiles(Layout$,mat DirList$;BypassExtension$,___,Dummy$*255,Dirfile,LayoutPath$*128,Star$)
       layoutpath$=fnSettings$("layoutpath")

       star$=fnNeedStar$

       library : Fngetfilenumber

       mat Dirlist$(0)
       execute star$&"dir "&layoutPath$&"version\"&Layout$&"* >tmpdir[session]"

       open #(Dirfile:=Fngetfilenumber) : "Name=tmpdir[session]", display,input
       ReadVersionFileName: ! Read The Next File Name
       linput #Dirfile: Dummy$ eof DoneReadVersionFiles
       if fnTestVersionHistoryFile(Dummy$,BypassExtension$) then
          mat dirlist$(udim(dirlist$)+1)
          dummy$=trim$(dummy$(44:99))
          dirlist$(udim(dirlist$))=dummy$(1:30)
       end if
       goto ReadVersionFileName
       DoneReadVersionFiles: ! Finished
       close #Dirfile:

       execute Star$&"free tmpdir[session]"
    fnend

    def fnTestVersionHistoryFile(Line$*255;BypassExtension$)
       if BypassExtension$="" or trim$(uprc$(Line$(11:13)))><trim$(uprc$(BypassExtension$(2:4))) then
          if ~pos(Line$,"<DIR>") and Line$(1:13)><"Directory of " and ~pos(Line$,"Kilobytes Used,") then
             fnTestVersionHistoryFile=1
          end if
       end if
    fnend
 !
 DOESLAYOUTEXIST: ! Return True If Layout Exists In The Layouts Folder
    def library Fndoeslayoutexist(Layout$;___,Layoutpath$*255,FileLayoutExtension$)
       layoutpath$=fnSettings$("layoutpath")
       fileLayoutExtension$=fnSettings$("layoutextension")
       layout$=trim$(Layout$)
       
       if trim$(layout$)<>"" then
          fndoeslayoutexist = exists(layoutpath$&layout$&FileLayoutExtension$)
       end if
    fnend

 LayoutPath: ! This funtion reads the layout path
    def library FnReadLayoutPath$*255=fnSettings$("layoutpath")
    def library FnReadLayoutExtension$=fnSettings$("layoutextension")

 ReadTemplates: ! ***** Reads Into A Given Array The List Of File Layouts
    dim TemplateList$(1)
    dim TemplateDescription$(1)*255
    def fnReadTemplates(Mat Dirlist$,mat TemplateDesc$,mat TemplateNum;___,Dummy$*255,Dirfile,TemplatePath$*255,Index,Spot,Jndex,Star$)
       star$=fnNeedStar$
       library : Fngetfilenumber
       templatePath$=fnSettings$("templatepath")
       mat TemplateList$(0)
       execute Star$&"dir "&TemplatePath$&" >xxx" error Ignore
       if exists("xxx") then
          open #(Dirfile:=Fngetfilenumber) : "Name=xxx", display,input
          READtemplateNAME: ! Read The Next File Name
          linput #Dirfile: Dummy$ eof DONEREADINGtemplateNAMES
          if (trim$(lwrc$(Dummy$(11:13)))="wb" or trim$(lwrc$(Dummy$(11:13)))="br") And ~Pos(Dummy$,"<DIR>") then
             mat TemplateList$(Udim(TemplateList$)+1) !:
             templateList$(Udim(TemplateList$))=Trim$(Dummy$(44:99))(1:30)
          end if
          goto READtemplateNAME
          DONEREADINGtemplateNAMES: ! Finished
          close #Dirfile:
          execute Star$&"free xxx"
       end if

       mat DirList$(0) : mat TemplateDesc$(0) : mat TemplateNum(0)
       for Index=1 to udim(mat TemplateList$)
          ! attempt library linkage

          library TemplatePath$&TemplateList$(Index) : fnTemplateList, fnRunTemplate error SkipThisTemplate
          fnTemplateList(mat TemplateDescription$) error SkipThisTemplate

          spot=udim(mat DirList$)
          mat DirList$(udim(mat TemplateDescription$)+Spot)
          mat DirList$(Spot+1:Spot+udim(mat TemplateDescription$))=(TemplatePath$&TemplateList$(Index))
          mat TemplateDesc$(udim(mat TemplateDescription$)+Spot)
          mat TemplateDesc$(Spot+1:Spot+udim(mat TemplateDescription$))=TemplateDescription$
          mat TemplateNum(udim(mat TemplateDescription$)+Spot)
          for Jndex=1 to udim(mat TemplateDescription$)
             templateNum(Spot+Jndex)=Jndex
          next Jndex
          SkipThisTemplate: ! Skip to here if a file isn't a template.
       next Index
    fnend
    
    ! Ignore layouts we're supposed to ignore.
    def fnIgnoreLayouts(mat LvDir_Listing$;___,_IgnoreLayouts$*255,dc_Index,dc_Jndex)
       _IgnoreLayouts$=fnSettings$("ignorelayouts")
       for dc_Index=1 to udim(mat lvdir_listing$)
          if ~pos(lwrc$(_IgnoreLayouts$),lwrc$(trim$(lvdir_listing$(dc_index)))) then
             dc_Jndex+=1
             lvDir_Listing$(dc_Jndex)=lvDir_Listing$(dc_Index)
          end if
       next dc_Index
       mat lvDir_Listing$(dc_Jndex)
    fnend
 !
 ! #Autonumber# 50000,10
 RenameLayouts: ! Rename Layouts
    fnRenameLayoutExtension
    if fkey=93 then execute "system"
    stop
    
    dim TmpDirList$(1)*128
    def fnCheckForNewExtension(mat LvDir_Listing$)
       if udim(mat Lvdir_Listing$)=0 and len(trim$(fnSettings$("layoutextension")))>0  then
          fnReadLayouts(Mat TmpDirList$,".")
          if udim(mat TmpDirList$) then
             dim LongMessage$*10000
             longMessage$="I'm not seeing any layouts but I found some using your old layout extension. Would you like to "
             longMessage$(99999:0)="run the Rename Layout Extension Wizard to rename all your layout files to use the new "
             longMessage$(99999:0)="layout extension? (You can run the Rename Layout Extension Wizard at any time by loading "
             longMessage$(99999:0)="fileio and typing ""run 50000"", though you should only do that when you actually want to change the extension.)"
             if (2==msgbox(LongMessage$,"New Layout Extension Detected","yN","QST")) then
                count=fnRenameLayoutExtension
                fnreadlayouts(Mat Lvdir_Listing$)
                msgbox("We renamed "&str$(Count)&" layouts and layout history files. Please test every data file in your system and then archive your old file layouts so you don't accidentally change the wrong ones.","Upgrade Complete")
             end if
          end if
       end if
    fnend

    def fnRenameLayoutExtension(;___,Count)
       library : fnGetFileNumber
       toExt$=fnSettings$("layoutextension")
       fromExt$="."
       open #(Window:=fnGetFileNumber): "srow=10,scol=30,rows=3,cols=25,border=s,caption=Change Extension",display,outIn
       pr #Window, fields "1,1,CR 10;2,1,CR 10" : "From Ext: ", "To Ext: "
       pr #Window, fields "3,1,CC 25" : "Press Enter to Continue"
       do
          rinput #Window, fields "1,12,V 10,/W:W;2,12,V 10,/W:W" : FromExt$,ToExt$
       loop until fkey=99 or fkey=93 or fkey=0
       close #Window:
       execute "con gui off"
       if fkey=0 then count=fnRenameLayouts(FromExt$,ToExt$)
       execute "con gui on"
       fnRenameLayoutExtension=Count
    fnend

    dim LayoutRename$(1)*128
    dim VersionRename$(1)*128
    def fnRenameLayouts(FromExt$,ToExt$;___,Index,Jndex,LayoutPath$*128,Count)
       layoutpath$=fnSettings$("layoutpath")
       library : fnReadLayouts, fnDirVersionHistoryFiles
       fnReadLayouts(mat LayoutRename$,FromExt$)
       for Index=1 to udim(mat LayoutRename$)
          if ~exists(LayoutPath$&LayoutRename$(Index)&ToExt$) then
             execute "copy """&LayoutPath$&LayoutRename$(Index)&FromExt$&""" """&LayoutPath$&LayoutRename$(Index)&ToExt$&""""
             count+=1
          else
             pr "Bypassing "&LayoutPath$&LayoutRename$(Index)&ToExt$
          end if
          fnDirVersionHistoryFiles(LayoutRename$(Index),mat VersionRename$,ToExt$)
          for Jndex=1 to udim(mat VersionRename$)
             if ~exists(LayoutPath$&"version\"&VersionRename$(Jndex)&ToExt$) then
                execute "copy """&LayoutPath$&"version\"&VersionRename$(Jndex)&FromExt$&""" """&LayoutPath$&"version\"&VersionRename$(Jndex)&ToExt$&""""
                count+=1
             else
                pr "Bypassing "&LayoutPath$&"version\"&VersionRename$(Jndex)&ToExt$
             end if
          next Jndex
       next Index
       pr "Copied "&str$(Count)&" files."
       fnRenameLayouts=Count
    fnend

 MESSAGEBOX: ! ***** Displays A Message Box In New Or Old Gui
    def Fnmessagebox(Message$*60;Title$*20,_Window,_Choice,_Kp$)
       library : Fngetfilenumber
       if Trim$(Env$("guimode"))="" then ! Old Gui Version
          open #(_Window:=Fngetfilenumber): "SROW=10,SCOL=10,EROW=14,ECOL=69,Border=S,Caption="&Title$,display,outIn
          pr #_Window: Newpage
          pr #_Window, fields "2,1,CC 60" : Message$
          pr #_Window, fields "4,1,CR 29" : "Answer (Y/N):"
          do
             rinput #_Window, fields "4,31,C 1" : _Kp$
          loop Until (Fkey=0 And (Uprc$(_Kp$)="Y" Or Uprc$(_Kp$)="N")) Or Fkey=99
          if Fkey=0 And Uprc$(_Kp$)="Y" then let Fnmessagebox=1 else let Fnmessagebox=0
          close #_Window:
       else ! New Gui Version
          _Choice=Msgbox(Message$,Title$,"okC")
          if _Choice=1 then let Fnmessagebox=1 else let Fnmessagebox=0
       end if
    fnend
       
 SAMEAS: !   ***** Compares Two String Arrays Returning True If Same
    def Fnsameas(Mat A$,Mat B$;___,Failed,Index)
       if Udim(Mat A$)=Udim(Mat B$) then
          for Index=1 to Udim(Mat A$)
             if Trim$(A$(Index))<>Trim$(B$(Index)) then failed=1
          next Index
          fnsameas=(~(Failed))
       end if
    fnend
 !
 SAMEA: !   ***** Compares  Two Number Arrays Returning True If Same
    def Fnsamea(Mat A,Mat B;___,Failed,Index)
       if Udim(Mat A)=Udim(Mat B) then
          for Index=1 to Udim(Mat A)
             if A(Index)<>B(Index) then failed=1
          next Index
          fnsamea=(~(Failed))
       end if
    fnend
 !
 KEY: ! ***** This Function Calculates The Key For A File
    def library Fnkey$*255(Filenumber, Key$*255)
       fnkey$=Rpad$(Key$,Kln(Filenumber))
    fnend

 SortKeys: ! This function will sort the records given in the mat Keys$
    ! array into the order specified by a given key file.

    dim SortKey$(1)*255
    dim Sorted(1)

    def library fnSortKeys(mat Keys$,Layout$,DataFile,mat F$,mat F,mat Form$;KeyNum,___,Index)
       library : fnBuildKey$
       mat SortKey$(udim(mat Keys$))
       mat Sorted(udim(mat Keys$))

       for Index=1 to udim(mat Keys$)
          read #DataFile, using form$(DataFile), key=keys$(Index), release : mat F$, mat F
          if file(DataFile)=0 then
             sortKey$(Index)=fnBuildKey$(Layout$,mat F$,mat F,KeyNum)
          end if
       next Index

       mat Sorted=aidx(SortKey$)
       mat SortKey$=Keys$

       for Index=1 to udim(mat Keys$)
          keys$=SortKey$(Sorted(Index))
       next Index
    fnend

    dim StatFile$(1)*255
    dim AuditKeys$(1)*255
    dim FileList$(1)*255
    dim statHeadings$(1)
    dim StatWidth(1)
    dim StatForm$(1)

 BeginAudit: ! ***** This function will create an Audit Comparison folder for all your data files.
    def library fnBeginAudit(;BackupFolder$*80,Path$*255,mat SelectedFiles$,___,Index,FileName$*255,Jndex)
       if trim$(BackupFolder$)="" then backupFolder$=fnSettings$("auditpath")
       library : fnCopyDataFiles
       fnCopyDataFiles(BackupFolder$,Path$,mat SelectedFiles$) ! ,2)
    fnend

    dim CallAuditFileList$(1)*255
    def fnCallAudit(Mat Sel,mat Files$;Path$*255,___,Index)

       ! Convert mat Selection and mat Files$ into mat Selected Files
       mat CallAuditFileList$(udim(mat Sel))
       for Index=1 to udim(mat CallAuditFileList$)
          callAuditFileList$(Index)=Files$(Sel(Index))
       next Index
       
       fnCompareFiles(mat CallAuditFileList$,Path$)
       fnSetFiles(mat CallAuditFileList$,Path$)
    fnend

    def fnSetFiles(;mat SelectedFiles$,Path$*255)
       library : fnBeginAudit
       fnBeginAudit("",Path$,mat SelectedFiles$)
    fnend
    
    def fnCompareFiles(;mat SelectedFiles$,Path$*255,___,BackupFolder$*255)
       backupFolder$=fnSettings$("auditpath")
       fnAddTrailingSlash(BackupFolder$)
       if exists(BackupFolder$) then
          library : fnCompare
          fnCompare(BackupFolder$,BackupFolder$&"audit.txt",1,0,Path$,mat SelectedFiles$)
       end if
    fnend
    
    Compare: ! ***** This function will compare two data sets
    def library FnCompare(BackupFolder$*80;Logfile$*80,Printer,DontClose,Path$*255,mat SelectedLayouts$,___,Index,NewFile,OldFile,OutFile,FileName$*255)
       dim FileList$(1)*255
       dim NewF$(1)*1024,NewF(1)
       dim OldF$(1)*1024,OldF(1)
       dim CompareForm$(1)*255
       dim Records(1)

       dim Dummy$(1)*1024
       dim Dummy(1)

       if trim$(BackupFolder$)="" then backupFolder$=fnSettings$("auditpath")

       fnAddTrailingSlash(BackupFolder$)
       fnAddTrailingSlash(Path$)
       if Path$="\" then path$=""

       library : fnReadLayouts, fnOpenFile, fnGetFileNumber, fnBuildKey$, FnReadLayoutHeader
       fnPrinterSys

       if len(trim$(LogFile$)) then
          open #(OutFile:=fnGetFileNumber): "name="&LogFile$&",recl=2048,use", display,output
       end if

       if Printer then
          open #255: "name=preview:/,recl=2048",display,output
          pr #255: "[Landscape][A4PAPER][SetFont(Lucida Console)][CPI(15)]"
       end if

       if OutFile then pr #OutFile: "File Compare started at "&Time$&" on "&Date$("month dd, ccyy")
       if Printer then pr #255: "File Compare started at "&Time$&" on "&Date$("month dd, ccyy")

       if ~exists(BackupFolder$) then
          if OutFile then pr #OutFile: "Backup Folder not Found."
          if Printer then pr #255: "Backup Folder not Found."
       else
          if fnValidMat(mat SelectedLayouts$) then
             mat FileList$(udim(SelectedLayouts$))=SelectedLayouts$
          else
             fnReadLayouts(mat FileList$)
          end if

          for Index=1 to udim(mat FileList$)
             fnReadLayoutHeader(FileList$(Index),FileName$)
             if exists(Path$&FileName$) and exists(BackupPath$&Path$&FileName$) then

                newFile=fnOpen(FileList$(Index),mat NewF$,mat NewF,mat Compareform$,1,0,0,Path$,mat Dummy$,mat Dummy,1)
                if NewFile then
                   oldFile=fnOpen(FileList$(Index),mat OldF$,mat OldF,mat Compareform$,1,0,0,BackupFolder$&Path$,mat Dummy$,mat Dummy,1)
                   if OldFile then

                      if OutFile then pr #OutFile: "Comparing File: "&FileList$(Index)
                      if Printer then pr #255: "Comparing File: "&FileList$(Index)

                      mat Records(0)=(0)
                      do until file(NewFile)=10 ! eof
                         read #NewFile, using Compareform$(NewFile) : mat NewF$,mat NewF error Ignore
                         if file(NewFile)=0 then
                            Mat Records(udim(Mat Records)+1)
                            records(udim(Mat Records))=rec(NewFile)

                            read #OldFile, using Compareform$(OldFile), rec=rec(NewFile) : mat OldF$,mat OldF noRec Ignore

                            if File(OldFile) then
                               ! Log a Record is Added message
                               if OutFile then
                                  pr #Outfile: "Record "&str$(rec(NewFile))&" was added with key: '"&fnBuildKey$(FileList$(Index),mat NewF$,mat NewF)&"', The Data was:";
                                  fnPrintArrays$(OutFile,mat NewF$,mat NewF)
                               end if

                               if Printer then
                                  pr #255: "[GREEN]Record "&str$(rec(NewFile))&" was added with key: '"&fnBuildKey$(FileList$(Index),mat NewF$,mat NewF)&"', The Data was:";
                                  fnPrintArrays$(255,mat NewF$,mat NewF)
                                  pr #255: "[BLACK]";
                               end if
                            else
                               ! Compare two records, log both records
                               if fnSameas(mat NewF$,mat OldF$) and fnSameA(mat NewF,mat OldF) then
                                  ! Do nothing, we're fine
                               else
                                  if OutFile then
                                     pr #Outfile: "Record "&str$(rec(NewFile))&" with key: '"&fnBuildKey$(FileList$(Index),mat NewF$,mat NewF)&"' was changed."
                                     pr #OutFile: "The Data was: ";
                                     fnPrintArrays$(OutFile,mat OldF$,mat OldF)
                                     pr #OutFile: "   It is now: ";
                                     fnPrintArrays$(Outfile,mat NewF$,mat NewF)
                                  end if

                                  if Printer then
                                     pr #255: "Record "&str$(rec(NewFile))&" with key: '"&fnBuildKey$(FileList$(Index),mat NewF$,mat NewF)&"' was changed."
                                     pr #255: "The Data was: ";
                                     fnPrintArrays$(255,mat OldF$,mat OldF,1,mat NewF$,mat NewF,14)
                                     pr #255: "   It is now: ";
                                     fnPrintArrays$(255,mat NewF$,mat NewF,1,mat OldF$,mat OldF,14)
                                  end if
                               end if
                            end if
                         else if file(newfile)<>10 then
                            ! Log an Error Reading File message
                            if OutFile then pr #Outfile: "Warning: Error Reading Record "&str$(rec(NewFile))&" in the current data. Record Skipped."
                            if Printer then pr #255: "[RED]Warning: Error Reading Record "&str$(rec(NewFile))&" in the current data. Record Skipped.[BLACK]"
                         end if
                      loop
                      restore #OldFile:
                      do until file(OldFile)=10 ! Eof
                         read #OldFile, using CompareForm$(OldFile) : mat OldF$,mat OldF error Ignore
                         if file(OldFile)=0 then
                            if srch(Records,rec(OldFile))<1 then
                               if OutFile then
                                  pr #Outfile: "Record "&str$(rec(OldFile))&" was deleted with key: '"&fnBuildKey$(FileList$(Index),mat OldF$,mat OldF)&"', The Data was:"
                                  fnPrintArrays$(OutFile,mat OldF$,mat OldF)
                               end if
                               if Printer then
                                  pr #255: "[RED]Record "&str$(rec(OldFile))&" was deleted with key: '"&fnBuildKey$(FileList$(Index),mat OldF$,mat OldF)&"', The Data was:"
                                  fnPrintArrays$(255,mat OldF$,mat OldF)
                                  pr #255: "[BLACK]";
                               end if
                            end if
                         else if file(OldFile)<>10 then
                            ! Log an Error Reading File message
                            if OutFile then
                               pr #Outfile: "Warning: Error Reading Record "&str$(rec(OldFile))&" in the history data. Record Skipped."
                            end if

                            if Printer then
                               pr #255: "[RED]Warning: Error Reading Record "&str$(rec(OldFile))&" in the history data. Record Skipped.[BLACK]"
                            end if
                         end if
                      loop

                      Close #OldFile:
                   end if
                   Close #NewFile:
                end if
             else
                if ~exists(Path$&FileName$) then
                   if OutFile then pr #OutFile: "Cannot Compare File: "&FileList$(Index)&" - The Data File doesn't exist"
                   if Printer then pr #255: "Cannot Compare File: "&FileList$(Index)&" - The Data File doesn't exist"
                end if
                if ~exists(BackupPath$&Path$&FileName$) then
                   if OutFile then pr #OutFile: "Cannot Compare File: "&FileList$(Index)&" - The Backup of the File doesn't exist."
                   if Printer then pr #255: "Cannot Compare File: "&FileList$(Index)&" - The Backup of the File doesn't exist."
                end if
             end if
          next Index
       end if

       if OutFile then Close #OutFile:
       if Printer and ~DontClose then Close #255:

       fnCompare=1
    fnend

    def fnPrintArrays$(FileNumber,mat F$,mat F;Color,mat Of$,mat Of,Length,___,Index)
       for Index=1 to udim(mat F$)
          length+=len(f$(Index))+2
          if Length>120 and FileNumber=255 then pr #FileNumber: : length=0
          if Color and trim$(Of$(Index))<>trim$(F$(Index)) then
             pr #FileNumber: "[Red]"&trim$(F$(Index))&", "&"[Black]";
          else
             pr #FileNumber: trim$(F$(Index))&", ";
          end if
       next Index
       for Index=1 to udim(mat F)
          length+=len(str$(f(Index)))+2
          if Length>128 and FileNumber=255 then pr #FileNumber: : length=0
          if Color and Of(Index)<>F(Index) then
             pr #FileNumber: "[Red]"&str$(F(Index))&", "&"[Black]";
          else
             pr #FileNumber: str$(F(Index))&", ";
          end if
       next Index
       pr #Filenumber:
    fnend

    dim PrinterSysSet
    def fnPrinterSys ! Activate Printer.Sys commands
       if ~PrinterSysSet then
          execute "config PRINTER TYPE NWP select WIN:"
          execute "config PRINTER TYPE NWP select PREVIEW:"

          execute 'config PRINTER NWP [RED], "\Ecolor=''#A00000''"'
          execute 'config PRINTER NWP [GREEN], "\Ecolor=''#00A000''"'
          execute 'config PRINTER NWP [BLACK], "\Ecolor=''#000000''"'
          execute 'config PRINTER NWP [CPI(NNN)], "\E(sNNNH"'
          execute 'config PRINTER NWP [SETFONT(FontName)], "\Efont=''FontName''"'
          execute 'config PRINTER NWP [A4PAPER], "\E&l26A" ! 210mm x 297mm'
          execute 'config PRINTER NWP [LANDSCAPE], "\E&l1O"'
          printerSysSet=1
       end if
    fnend

    def fnAddTrailingSlash(&Folder$)
       if Folder$(len(Folder$):len(Folder$))<>"\" then
          folder$=Folder$&"\"
       end if
    fnend

    def fnValidMat(mat X$;___,Index,Valid)  ! Checks to see if the given string array has any layouts in it
       do while ((Index:=Index+1)<=udim(mat X$))
          if len(X$(Index)) then Valid=1
       loop until Valid
       fnValidMat=Valid
    fnend

    ! This function will copy ALL your data files to a backup folder.
    def library fnCopyDataFiles(BackupFolder$*255;Path$*255,mat SelectedLayouts$,SkipKeys,___,Index,FileName$*255,Jndex,Status,StatusCount,Dummy)
       library : fnReadLayouts, FnReadLayoutHeader, fnCopyFile, fnGetFileNumber

       fnAddTrailingSlash(BackupFolder$)
       fnAddTrailingSlash(Path$)
       if Path$="\" then path$=""

       if ~exists(BackupFolder$) then execute "mkdir "&BackupFolder$

       open #(status:=fnGetFileNumber): "srow=5,scol=10,rows=15,cols=60, border=s, caption=Progress",display,outIn

       statHeadings$(1)="File"
       statWidth(1)=58
       statForm$(1)="CC 255"

       pr #status, fields "1,1,LIST 15/60,HEADERS,/W:W" : (mat StatHeadings$,mat StatWidth, mat StatForm$)

       if fnValidMat(mat SelectedLayouts$) then
          mat FileList$(udim(SelectedLayouts$))=SelectedLayouts$
       else
          fnReadLayouts(mat FileList$)
       end if

       for Index=1 to udim(Mat FileList$)
          if lwrc$(trim$(FileList$(Index)))<>"logfile" then
             fnReadLayoutHeader(FileList$(Index),FileName$,Mat AuditKeys$)
             if exists(Path$&Filename$) then
                fnMakeSurePathExists(Path$&fileName$,BackupFolder$)

                statFile$(1)=Path$&filename$
                pr #Status, fields "1,1,LIST 15/60,+" : mat StatFile$
                statusCount+=1
                curfld(1,StatusCount)
                input #Status, fields "1,1,LIST 15/60,ROWSUB,SEL,NOWAIT" : Dummy

                ! execute "copy "&FileName$&" "&BackupFolder$&FileName$&" -S -N"
                fnCopyFile(Path$&FileName$,Backupfolder$&Path$&FileName$)

                if SkipKeys=2 then ! 2 means clear, 1 means skip, 0 means include
                   for Jndex=1 to udim(Mat AuditKeys$)
                      if exists(BackupFolder$&Path$&AuditKeys$(Jndex)) then
                         execute "*free "&BackupFolder$&Path$&AuditKeys$(Jndex)
                      end if
                   next Jndex
                else
                   for Jndex=1 to udim(Mat AuditKeys$)
                      fnMakeSurePathExists(AuditKeys$(Jndex),BackupFolder$)

                      statFile$(1)=AuditKeys$(Jndex)
                      pr #Status, fields "1,1,LIST 15/60,+" : mat StatFile$
                      statusCount+=1
                      curfld(1,StatusCount)
                      input #Status, fields "1,1,LIST 15/60,ROWSUB,SEL,NOWAIT" : Dummy

                      ! execute "copy "&AuditKeys$(Jndex)&" "&BackupFolder$&AuditKeys$(Jndex)&" -S -N"
                      fnCopyFile(AuditKeys$(Jndex),BackupFolder$&Path$&AuditKeys$(Jndex))
                   next Jndex
                end if
             end if
          end if
       next Index

       close #Status:
    fnend

    dim Chunk$*10000
    def library fnCopyFile(FromFile$*255,ToFile$*255;NoProgressBar,___,FromFile,ToFile,Written)
       progressBar=~NoProgressBar

       library : fnGetFileNumber
       
       open #(FromFile:=fnGetFileNumber): "name="&FromFile$&",recl=10000,shr",external,input
       open #(ToFile:=fnGetFileNumber): "name="&ToFile$&",recl=10000,replace",external,output

       ReadNextRec: ! Read the next record and process it
          written=0
          read #FromFile, using CopyFile : Chunk$ eof DoneCopying ioerr ShortRec
          if ~Written then write #ToFile, using CopyFile : Chunk$

          if ProgressBar and lrec(FromFile) then let fnUpdateProgressBar(rec(FromFile)/lrec(FromFile),"",0,0,0,"Transferring '"&FromFile$&"' File")
       goto ReadNextRec

       DoneCopying: ! Done Copying the file here

       if ProgressBar then let fnCloseProgressBar

       close #FromFile:
       close #ToFile:
    fnend

    CopyFile: form C 10000

    ShortRec: ! Process short record at EOF
       c=Cnt
       if err><4271 then let msgbox("Error during copying. Please try again.") : goto DoneCopying
       reread #FromFile, using CopyFile : Chunk$
       ! chunk$=rtrm$(Chunk$,chr$(0))
       rln(ToFile,C)
       write #ToFile, using "form C "&str$(C) : Chunk$(1:C)
       written=1
    continue


    dim PBarSpec$(1),PBarDot$(1)
    dim pb_StartTime,pb_LastPercent,pb_Window,pb_NewGui,pb_pTime,pb_Size,pb_Color$
    dim pb_PrintedLastMessageRow$*255,pb_OpenedMessageRows

    def library fnProgressBar(Percent;Color$,ProgressAfter,ProgressThreshhold,UpdateThreshhold,Caption$*255,MessageRow$*255)=fnUpdateProgressBar(Percent,Color$,ProgressAfter,ProgressThreshhold,UpdateThreshhold,Caption$,MessageRow$)
    def library fnCloseBar=fnCloseProgressBar
    def fnUpdateProgressBar(Percent;Color$,ProgressAfter,ProgressThreshhold,UpdateThreshhold,Caption$*255,MessageRow$*255,___,MessageRows,Index,FromSpot,ToSpot,SRow,SCol,Rows,Cols)
       ! Initialize Defaults
       if ~ProgressAfter then progressAfter=.8            ! 3/4ths of a Second
       if ~ProgressThreshhold then progressThreshhold=.45 ! About 45 percent.
       if ~UpdateThreshhold then updateThreshhold=.4      ! 4/10th of a Second

       if len(Caption$) then caption$=",Caption="&Caption$

       ! Save Color
       if Len(Color$) then pb_Color$=Color$
       if ~len(pb_Color$) then pb_Color$="#00FF00"

       ! Initialize Start Time
       if ~pb_StartTime then pb_StartTime=Timer
       
       if pb_Window and pb_OpenedMessageRows and MessageRow$><pb_PrintedLastMessageRow$ then
          pr #pb_Window, fields "2,1,"&str$(pb_Size)&"/CC 255" : MessageRow$
          messageRow$=pb_PrintedLastMessageRow$
       end if
       
       if fnProgressBarTime(pb_StartTime,ProgressAfter,Percent,ProgressThreshhold) then ! If its time to open the progress bar
          if ~pb_Window then ! if the pb_window isn't open yet, check if we need to open it
             ! Open Window
             
             ! First, Detect Size and Position.
             fnReadScreenS(Rows,Cols)
             pb_Size=int(Cols*.6666)    ! 78 Size should be .666*ScreenWidth
             sRow=int(Rows/2)           ! 11 SRow should be int(half of the window) - 1
             sCol=int((Cols-pb_Size)/2) ! 2  SCol should be int(ScreenWidth-Size/2)
             messageRows=1 : pb_OpenedMessageRows=0
             if len(MessageRow$) then messageRows+=1 : pb_OpenedMessageRows=1

             open #(pb_Window:=fnGetFileNumber): "SRow="&str$(SRow)&",SCol="&str$(SCol)&",Rows="&str$(MessageRows)&",Cols="&str$(pb_Size)&Caption$&",Border=S",display,outIn

             ! Initialize Window, Detect Gui Mode, Configure dot positions.
             if env$("guimode")="ON" then
                pb_NewGui=1
             else
                mat PBarSpec$(pb_Size) : mat PBarDot$(pb_Size)
                for Index=1 to pb_Size
                   pBarSpec$(Index)="1,"&Str$(Index)&",C 1"
                next Index
                mat PBarDot$=(".")
                pr #pb_Window, fields mat PBarSpec$ : mat PBarDot$
                mat PBarDot$=("o")
             end if
          end if
          
          if pb_Window and pb_pTime+UpdateThreshhold<Timer then ! We're printing already, and its been long enough.
             if (Percent*pb_Size)-pb_LastPercent>=1 then
                fromSpot=Max(int(pb_LastPercent),1)
                toSpot=Min(Int(Percent*pb_Size),pb_Size)
                if pb_NewGui then
                   pr #pb_Window, fields "1,1,"&str$(ToSpot)&"/C 1,S/W:"&pb_Color$ : ""   ! Probably much faster in new gui, a single control.
                else
                   pr #pb_Window, fields mat PBarSpec$(FromSpot:ToSpot) : mat PBarDot$(FromSpot:ToSpot)
                end if
                pb_LastPercent=Percent*pb_Size
             end if
             pb_pTime=Timer
          end if
       end if
    fnend

    def fnCloseProgressBar(;___,FromSpot,ToSpot)
       if pb_Window then
          ! pr end of Progress Bar
          fromSpot=Max(int(pb_Lastpercent),1)
          toSpot=pb_Size
          if FromSpot<=ToSpot then
             if pb_NewGui then
                pr #pb_Window, fields "1,1,"&str$(ToSpot)&"/C 1,S/W:"&pb_Color$ : ""
             else
                pr #pb_Window, fields mat PBarSpec$(FromSpot:ToSpot) : mat PBarDot$(FromSpot:ToSpot)
             end if
          end if
          ! Close Progress Bar
          close #pb_Window:
       end if

       ! Also reset all progress bar saved parms for next time.
       pb_Window=pb_LastPercent=pb_StartTime=pb_NewGui=pb_pTime=pb_Size=0
    fnend

    ! Progress Bar Calculations. fnEnoughProgress is the basic formula. fnProgressBarTime calls it with 1, 2, and 3 for Multipliers.
    def fnEnoughProgress(Multiplier,StartTime,ProgressAfter,Percent,Threshhold)=((StartTime+(ProgressAfter*Multiplier))<Timer and Percent<(Threshhold*Multiplier))
    def fnProgressBarTime(ST,PA,PCT,T)=(fnEnoughProgress(1,ST,PA,PCT,T) or fnEnoughProgress(2,ST,PA,PCT,T) or fnEnoughProgress(3,ST,PA,PCT,T))

    def fnMakeSurePathExists(Filename$*255;Path$*255)
       do while pos(FileName$,"\")
          path$=Path$&FileName$(1:pos(FileName$,"\"))
          fileName$=fileName$(pos(FileName$,"\")+1:len(Filename$))
          if ~Exists(Path$) then execute "mkdir "&path$
       loop
    fnend

    def library fnShowMessage(Message$*54;___,WinNumber)
       library : fnGetFileNumber
       open #(WinNumber:=fnGetFileNumber): "SROW=10,SCOL=12,ROWS=3,COLS=56,BORDER=S",display,outIn
       pr #WinNumber, fields "2,2,CC 54" : Message$
       fnShowMessage=WinNumber
    fnend

    ! This function reads an environment variable on the client under client server.
    def library fnClientEnv$*255(EnvKey$;___,F,Value$*255)
       library : fnGetFileNumber
       ! Make a cmd file
       open #(F:=fnGetFileNumber): "name=@:temp[session].cmd, replace", display, output
       pr #F: "echo %"&EnvKey$&"% > tempout"&session$&".txt"
       close #f:
       execute "system -@ -M temp"&session$&".cmd"
       open #(F:=fnGetFileNumber): "name=@:tempout[session].txt", display, input
       linput #f: Value$
       close #f:
       fnClientEnv$=rtrm$(Value$)
    fnend

    dim rowval$(1)*255,printrow$*1000

    dim start(1),ender(1),mask(1)
    DIM HEADERS$(1)*50,EXWIDTHS(1),EXFORMS$(1)*50,HEADERS$*2000, SortOrder(1)
    dim ListData$(1)*1023, ListRow$(1)*1023

    def library fnExportListviewCSV(Window,SPEC$;GenFileName,Delim$,Filename$*255,___,WaitWin,CS,NumOfCols,turn,instance,n,m,SortNext,Format$*64)
       if fn43 then
          library : fnGetFileNumber,fnShowMessage

          if ~len(trim$(Delim$)) then delim$='","'
          waitWin=fnShowMessage("Exporting... Please Wait.")

          ! Query the listview Header
          INPUT #Window, FIELDS SPEC$&",HEADERS,,nowait ": (MAT HEADERS$,MAT EXWIDTHS,MAT EXFORMS$)
          mat2str(Mat Headers$,Headers$,'","')

          ! Read the Listview Filter
          input #Window, fields spec$&",mask,displayed_order,nowait" : (mat mask)
          ! Read number of columns
          INPUT #Window, FIELDS SPEC$&",colcnt,all,nowait" : numofcols

          ! Handle any Sorting that has occurred.
          INPUT #Window, FIELDS SPEC$&",rowsub,all,displayed_order,nowait": MAT SortOrder


          ! Calculate the mat Start and mat End necessary to read the data
          ! We're using a trick here. Looks like BR Returns displayed_order with all the
          !  filter-showing rows at the end. So we just read the last however many rows
          !  of our mat SortOrder and use it for our Starts and Ends, and they're already in
          !  displayed order for us
          mat start(sum(mask))=SortOrder(udim(mat SortOrder)-sum(mask)+1:udim(mat SortOrder))
          mat ender(sum(mask))=start

          ! Make enough room for the data we're reading
          mat ListData$(numofcols*udim(mat start))
          mat ListRow$(numofcols)

          ! Read the data in the order sorted
          for n=1 to udim(mat Start)
             input #Window, fields spec$&",row,range,nowait" : start(n), ender(n), (mat ListRow$)
             mat ListData$(((n-1)*numofcols)+1:n*numofcols)=ListRow$
          next n

          ! Find a unique name
          if GenFileName then
             fileName$="@:csvexpt"&Session$&date$("mmdddyy")&time$(1:2)&time$(4:5)&time$(7:8)&".csv"
             do while exists(FileName$)
                instance+=1
             loop
             nocsv=len(filename$)-4
             fileName$=FileName$(1:nocsv)&str$(Instance)&".csv"
          end if

          ! Open the data file
          if (len(trim$(Filename$))) then
             open #(CS:=fnGetFileNumber): "NAME="&Filename$&",replace,recl=2000",display,output
          else
             OPEN #(CS:=fnGetFileNumber): "NAME=SAVE:CSV files (*.csv) |*.csv,replace,RECL=2000", display, output
             fileName$=file$(CS)
          end if

          ! pr Header Line
          pr #CS: '"'&Headers$&'"'

          ! Loop through data and pr the CSV file one line at a time.
          turn=0
          do
             mat ListRow$(numofcols)
             for n=1 to numofcols
                listRow$(n)=trim$(listdata$(n+turn))
                ! Apply Conversion if needed
                if lwrc$(trim$(ExForms$(n)(1:5)))="date(" or lwrc$(trim$(ExForms$(n)(1:4)))="fmt(" or lwrc$(trim$(ExForms$(n)(1:4)))="pic(" then
                   format$=ExForms$(n)
                   if pos(Format$,",") then
                      format$=Format$(1:pos(Format$,",")-1)
                   end if
                   listRow$(n)=cnvrt$(Format$,val(ListRow$(n))) error Ignore
                end if
             next n
             m=0
             for n=1 to numofcols
                m+=1
                if n<=udim(mat exwidths) and exwidths(n)=0 then
                   fnRemoveColumn(m,mat ListRow$)
                   m-=1
                end if
             next n

             mat2str(mat ListRow$,printrow$,delim$)
             pr #CS: '"'&printrow$&'"'

             printrow$=""
             turn=turn+numofcols
          loop while turn<(numofcols*udim(mat Start))

          close #CS:

          ! Launch the file in the default application ..
          execute "sy -C -M start "&os_filename$(FileName$)
          Close #Waitwin:
       else
          msgbox("Export from Listview requires BR 4.3 or higher.","Export Failed")
       end if
    fnend

    def fnRemoveColumn(col,mat Array$;___,i)
       for i=Col to udim(mat Array$)-1
          array$(i)=Array$(i+1)
       next i
       mat Array$(udim(mat Array$)-1)
    fnend


    dim EmailConf$(1)*1024,EmailConf(1),EmailConf,form$(1)*255
    dim CCEmailString$*10000
    dim EmailAddressString$*10256
    def library FnSendEmail(Emailaddress$*255,EmailMessage$*10000;Subject$*255,Invoicefile$*255,noprompt,BCCEmail$*255,mat CCEmails$,CCAsTo,___,Resultfile,Success,Outputstring$*1024,Index)

       library "fileio" : fnOpenFile, fnGetFileNumber, fnDoesLayoutExist
       if fnDoesLayoutExist("emailcfg") then
          if exists("sendemail.exe") then
             emailConf=fnOpen("emailcfg",mat EmailConf$,mat EmailConf,mat Form$,1,0,0,"",d$,D,0,0,1)
             read #EmailConf, using form$(EmailConf),rec=1 : mat EmailConf$,mat EmailConf noRec Ignore
             if file(EmailConf)=0 then
                if Exists("EmailLog.[WSID]") then execute "*free EmailLog.[WSID]"
                if EmailMessage$="" then emailMessage$=" "
                if len(BCCEmail$) then bCCEmail$(1:0)=" -bcc "
                cCEmailString$=""
                for Index=1 to udim(mat CCEmails$)
                   if len(trim$(CCEmails$(Index))) then
                      if len(trim$(CCEmailString$)) then cCEmailString$=CCEmailString$&" "
                      cCEmailString$=CCEmailString$&CCEmails$(Index)
                   end if
                next Index

                emailAddressString$=Emailaddress$

                if CCAsTo then
                   if len(trim$(EmailAddressString$)) then emailAddressString$=EmailAddressString$&" "
                   emailAddressString$=EmailAddressString$&CCEmailString$
                   cCEmailString$=""
                end if

                if len(trim$(CCEmailString$)) then cCEmailString$=" -cc "&CCEmailString$
                
                
                
                if len(trim$(InvoiceFile$)) then
                   execute 'sy -M -s sendemail -s '&EmailConf$(conf_server)&' -t '&EmailAddressString$&' -f '&EmailConf$(conf_fromaddr)&' -xu '&EmailConf$(conf_UserAccount)&' -xp '&EmailConf$(conf_password)&' -u "'&Subject$&'" -m "'&EmailMessage$&'" -a "'&os_filename$(InvoiceFile$)&'"'&CCEmailSTring$&BCCEmail$&' -v -q -l EmailLog.'&Wsid$
                else
                   execute 'sy -M -s sendemail -s '&EmailConf$(conf_server)&' -t '&EmailAddressString$&' -f '&EmailConf$(conf_fromaddr)&' -xu '&EmailConf$(conf_UserAccount)&' -xp '&EmailConf$(conf_password)&' -u "'&Subject$&'" -m "'&EmailMessage$&'"'&CCEmailSTring$&BCCEmail$&' -v -q -l EmailLog.'&Wsid$
                end if
                execute 'type EmailLog.[WSID] >>EmailLog.txt'
                open #(Resultfile:=Fngetfilenumber): "name=EmailLog.[WSID],recl=512",display,input
                do Until File(Resultfile)
                   linput #Resultfile: Outputstring$ eof IGNORE
                   if Pos(Outputstring$,"Email was sent successfully!") then success=1
                loop
                close #Resultfile:
                execute '*free EmailLog.[WSID]'
                fnSendEmail=Success
                if Success and ~noprompt then let msgbox("Email was sent successfully!","Email Sent")
             else
                msgbox("Email account not configured. Please enter the appropriate information in the emailcfg file.")
             end if
          else
             msgbox("SendEmail.exe program not found. Please include it with your software. This program can be downloaded for free from the internet.")
          end if
       else
          msgbox("The Email Configuration File is missing. Please add the emailcfg file layout to your layout folder. It is included in the latest versions of fileio.")
       end if
    fnend

 READSCREENSIZE: ! Return The Screen Size Of Window 0 In Terms Of Rows And Columns
    dim ScreenSize(4), FontSize(2)
    def library fnReadScreenSize(&Rows,&Cols;ParentWindow)=fnReadScreenS(Rows,Cols,ParentWindow)
    def fnReadScreenS(&Rows,&Cols;ParentWindow)
       if ~fn42e then
          fnOldReadScreenSize(Rows,Cols,ParentWindow)
       else
          file(ParentWindow,"USABLE_RECT",mat ScreenSize)
          file(ParentWindow,"FONTSIZE",mat FontSize)
          rows=(ScreenSize(4)/FontSize(1))
          cols=(ScreenSize(3)/FontSize(2))
       end if
    fnend

    def FnOldreadscreensize(&Rows,&Cols;ParentWindow,___,Ffile,String$*2000,Srow,Scol,Erow,Ecol,Position,Star$)
       star$=fnNeedStar$
       execute Star$&"Status Files >[SESSION].tmp"
       open #(Ffile:=Fngetfilenumber) : "name=[SESSION].tmp",display,input
 !
       do While File(Ffile)=0
          linput #Ffile: String$ eof IGNORE
       loop Until String$(1:5)="-----"
       do While File(Ffile)=0
          linput #Ffile: String$ eof IGNORE
       loop Until (Lwrc$(String$)(1:4)="open") And Trim$(String$(Pos(String$,"#")+1:Pos(String$,"#")+5))=str$(ParentWindow)
       if File(Ffile)=0 then
          linput #Ffile: String$ eof IGNORE
       end if
 !
       close #Ffile:
       execute Star$&"Free [SESSION].tmp"
 !
       if (Position:=Pos(Uprc$(String$),"SROW=")) then
          srow=Val(String$(Position+5:Pos(String$,",",Position)-1)) conv IGNORE
       end if
       if (Position:=Pos(Uprc$(String$),"SCOL=")) then
          scol=Val(String$(Position+5:Pos(String$,",",Position)-1)) conv IGNORE
       end if
       if (Position:=Pos(Uprc$(String$),"EROW=")) then
          erow=Val(String$(Position+5:Pos(String$,",",Position)-1)) conv IGNORE
       end if
       if (Position:=Pos(Uprc$(String$),"ECOL=")) then
          ecol=Val(String$(Position+5:Pos(String$,",",Position)-1)) conv IGNORE
       end if
       if Srow And Erow then rows=Erow-Srow+1
       if Scol And Ecol then cols=Ecol-Scol+1
    fnend

    def fn43=(val(wbversion$(1:3))>=4.3)
    def fn42e=(wbversion$>="4.20e")
    def fn42=(val(wbversion$(1:3))>=4.2)
    def fn418=((val(wbversion$(1:4))>=4.18) or fn42)
    def fn42ia=(wbversion$>="4.20ia")
    def fn42j=(wbversion$>="4.20j")
 !
 ! #Autonumber# 99000,10
 OPEN: ! ***** Function To Call Library Openfile And Proc Subs
       def Fnopen(Filename$*255, Mat F$, Mat F, Mat Form$; Inputonly, Keynum, Dont_Sort_Subs, Path$*255, Mat Descr$, Mat Field_Widths,Supress_Prompt,Ignore_Errors,Suppress_Log,___,Index)
          dim _FileIOSubs$(1)*800, _Loadedsubs$(1)*80
          fnopen=fnOpenFile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$,Suppress_Log)
          if Srch(_Loadedsubs$,Uprc$(Filename$))<=0 then : mat _Loadedsubs$(Udim(_Loadedsubs$)+1) : _Loadedsubs$(Udim(_Loadedsubs$))=Uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index
       fnend
 !
 ! #Autonumber# 99900,5
 IGNORE: continue
 RetryCount: !
    if ErrorRetryCount<5 then
       errorRetryCount+=1
       retry
    else
       errorRetryCount=0
       continue
    end if
    
 SleepRetryCount: !
    sleep(.1)
    if SleepRetryCount<5 then
       sleepRetryCount+=1
       retry
    else
       sleepRetryCount=0
       continue
    end if