00001  ! FileIO Library Basic Addon Template by Gabriel Bakker
00002  ! Copyright 2012 Gabriel Bakker, Sage AX
00003  ! Created: 2/6/2012
00004  !
00005  ! Use this template library as an example of how
00006  ! to construct other template libraries.
00007  !
00008  ! Every Template must have two library functions,
00009  !  fnTemplateList and fnRunTemplate.
00010  !
00011  !  fnTemplateList(mat TemplateDesc$)
00012  !  takes an array of descriptions and returns a list of
00013  !  the available functions in this template library.
00014  !
00015  !  fnRunTemplate(Template,FileLay$)
00016  !  this function takes the given template Index and
00017  !  file layout and calls your template function.
00018  !
00019  !  A template function should read the file layout
00020  !  and then build a string of code and return it
00021  !  in the clip board so that a programmer can
00022  !  paste it into his editor of choice later.
00023  !
00024  !
00025  ! To add to this template library, add your
00026  !  template description and template label
00027  !  to the list and gosub statement at the
00028  !  bottom of this library.
00029  !
00030  !  Then write some template code at that label
00031  !  in a subroutine that sets the code to return
00032  !  in the "ReturnCode$" variable, or use the
00033  !  fnReturnCode function.
00034  !
00035  dim Keys$(1)*255,KeyDescription$(1)*255,Ssubs$(1),Nsubs$(1),Sspec$(1),Nspec$(1)
00036  dim Sdescription$(1)*255,Ndescription$(1)*255,Spos(1),Npos(1)
00037  dim FileName$*255,Prefix$
00038  dim LongestElement
00039  !
00040  dim NumberList$
00041  dim KeyFields$(1)
00042  dim KeyGiven
00043  dim KeyLength, Length, Sub
00044  !
00045  dim CodeLine$*400
00046  !
00047  !
00048  !
00049  ERtn: ! r: Template for a ACS Error Routine
00050  fnReturnCode('! <updateable region: ertn>')
00051  fnReturnCode('ERTN: ')
00052  fnReturnCode('  fnerror(program$,err,line,act$,"xit")')
00053  fnReturnCode('  if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT')
00054  fnReturnCode('  if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT')
00055  fnReturnCode('  pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT')
00056  fnReturnCode('ERTN_EXEC_ACT: execute act$ : goto ERTN')
00057  fnReturnCode('! </updateable region: ertn>')
00058  return ! /r
00059  !
00060  dim ReturnCode$*20000
00061  def fnReturnCode(String$*512)
00062     returnCode$=ReturnCode$&String$&chr$(13)&chr$(10)
00063  fnend
00064  !
00065  def library fnRunTemplate(Template,FileLay$;___,Index)
00066     fnEstablishLinkage
00067     returnCode$=""
00068     fnReadEntireLayout(FileLay$,Filename$,Prefix$,Mat Keys$,Mat KeyDescription$,Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos)
00069     longestElement=0
00070     for Index=1 to udim(mat SSpec$)
00071        longestElement=max(LongestElement,fnLength(SSpec$(Index)))
00072     next Index
00073     for Index=1 to udim(mat NSpec$)
00074        longestElement=max(LongestElement,fnLength(NSpec$(Index)))
00075     next Index
00076     for Index=1 to udim(mat SSubs$)
00077        sSubs$(Index)=lwrc$(SSubs$(Index))
00078     next Index
00079     for Index=1 to udim(mat NSubs$)
00080        nSubs$(Index)=lwrc$(NSubs$(Index))
00081     next Index
00082     prefix$=trim$(Prefix$)
00083     gosub RunTemplate
00084  !
00085     if len(ReturnCode$) then let setenv("CLIPBOARD",ReturnCode$)
00086  fnend
00087  !
00088  RunTemplate: ! Template List gosub statement:
00089     on Template gosub ERtn
00090  return
00091  !
00092  TemplateList: ! Dictionary listing of available functions
00093     data "ERtn"
00094  !
00095  def library fnTemplateList(mat TemplateDesc$)
00096     restore TemplateList
00097     mat TemplateDesc$(1)
00098     read mat TemplateDesc$
00099  fnend
00100  !
00101  dim LinkageEstablished
00102  def fnEstablishLinkage
00103     if ~LinkageEstablished then
00104        library "fileio" : fnOpenFile, fnClose, fnReadEntireLayout, fnGetFileNumber, fnGetKeyElements, fnLength, fnAskCombo$
00105        linkageEstablished=1
00106     end if
00107  fnend
00108  !
99000  ! #Autonumber# 99000,10
99010  OPEN: ! ***** Function To Call Library Openfile And Proc Subs
99020        def Fnopen(Filename$*255, Mat F$, Mat F, Mat Form$; Inputonly, Keynum, Dont_Sort_Subs, Path$*255, Mat Descr$, Mat Field_Widths,Supress_Prompt,Ignore_Errors,___,Index)
99030           dim _FileIOSubs$(1)*800, _Loadedsubs$(1)*80
99040           fnopen=Fnopenfile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$)
99050           if Srch(_Loadedsubs$,Uprc$(Filename$))<=0 then : mat _Loadedsubs$(Udim(_Loadedsubs$)+1) : _Loadedsubs$(Udim(_Loadedsubs$))=Uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index
99060        fnend
99070  !
99080  Ignore: Continue
