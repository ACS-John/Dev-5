00010 !  Replace S:\Core\Search.br
00020 ! ______________________________________________________________________
00030   def library fnsearch(&cap$,file_num,&heading$,&form$,numeric_format$,&selection$,key_length)
00040 ! _________________________________________
00050 ! _________________________________________
00060 ! must have the following Dim statement in the main program                       DIM cap$*128,heading$*70,form$*80,numeric_format$*20
00070 ! file_num - use the file # of the file in your main program which is to          be searched
00080 ! heading$ - define as x characters - this is the heading on the search co      columns  - use format simular to: HEADING$=" Account #횼ame컴컴컴컴컴컴컴컴컴컴컴Meter Address컴컴횭alance"
00090 ! fs_info$(3)*30 - fs_info$(1) must always be the numeric key to the main       file - fs_info$(2) must always be the alpha index (name used for search) -      fs_info must always be numeric and be the 4th thing read - these are contr      olled by the form statement you send to this library
00100 ! form$*80  - form statement for reading master record - must be full form      at such as:  Form pos 1,c 10,pos 11,c 30,pos 89,c 10,pos 102,n 6  - must        have the key in the first position statement and a numeric in the fourth
00110 ! key_length - numeric LENGTH of the index used to access the actual            record in the main prgram
00120 ! you can have one column of numeric information on the search screen - it      must always be the fourth item - fs_info - enter the format that would be        used in a cnvrt$ statement after the open (   such as "pic(zz/zz/zz)"
00130 ! _________________________________________
00140     library 'S:\Core\Library': fnerror,fnwait,fncno,fnfkey,fnmsgbox,fnwin3b
00150 ! _________________________________________
00160     dim fs_sk$(22)*30,fs_bk$(99)*30,fs_sfl$(22),fs_sw$(22)*70,selection$*70,fs_info$(3)*30,numeric$*40,na1$*30
00165     selection$=''
00170 ! _________________________________________
00180 ASK_NAS: ! 
00190     alpha_key_length=kln(file_num)
00200     let win=104
00210     fnwin3b(win,cap$,4,45,0,2,5,2)
00220     bk1=prtall=0
00230     pr #win,fields "2,2,Cc 40,N": "Enter Search Criteria (blank for all):"
00240     key_position=22-round((alpha_key_length/2),0)
00250     na$(1)="3,"&str$(key_position)&",C "&str$(alpha_key_length)&",UT,N"
00260     input #win,fields mat na$: na1$
00270     if cmdkey=5 then goto CANCEL_OUT
00280 ! If CMDKEY><1 Then Goto 260
00290     close #win: 
00300     if len(rtrm$(na1$))=0 and len(rtrm$(na2$))=0 then prtall=1
00320 L320: fnwin3b(win,heading$,22,70,0,2,5,2)
00330     cde=0
00340     mat fs_sw$(22)
00350     for j=1 to 20
00360       if j>1 or selclp=1 then goto L380
00370       restore #file_num,search>=na1$(1:len(rtrm$(na1$))),release: nokey ASK_NAS
00380 L380: read #file_num,using form$,release: mat fs_info$,fs_info eof L490
00390       if prtall=1 then goto L420
00400       if len(rtrm$(na1$))=0 then goto L420
00410       if fs_info$(1)(1:len(rtrm$(na1$)))>na1$(1:len(rtrm$(na1$))) then goto L490
00420 L420: cde=cde+1
00425       numeric_value$="": numeric_value$=cnvrt$(numeric_format$,fs_info) conv L430
00430 L430: let fs_sw$(j)=(fs_info$(1)(1:key_length)&" "&fs_info$(2)(1:25)&" "&fs_info$(3)(1:12)&" "&numeric_value$)(1:70)
00440       let fs_sk$(j)=fs_info$(1)
00450       if j>1 then goto L480
00460       bk1=bk1+1
00470       if kps(file_num)=1 then let fs_bk$(bk1)=fs_info$(1) else let fs_bk$(bk1)=fs_info$(2) ! when backup a screen, use the numeric key if no alpha key being used for the search (eg G/L) (assuming numeric key starts in position 1)
00480 L480: next j
00490 L490: if cde=0 then goto L620
00500     mat fs_sw$(cde)
00510     for j=1 to 20 !:
            let fs_sfl$(j)=str$(j)&",1,C 70,N" !:
          next j
00520     mat fkey$=("") !:
          let fkey$(1)="Next" : let fkey$(2)="Back" : let fkey$(5)="Cancel" !:
          em$="": es=0 !:
          fnfkey(24,mat fkey$,mat disfk,em$,es)
00530     rinput #win,select mat fs_sfl$,attr "H": mat fs_sw$
00540     if cmdkey=5 then goto ASK_NAS
00550     if cmdkey=1 then goto L590
00560     if cmdkey=2 then goto BACK
00570     selection$=fs_sk$(curfld)
00580     if rtrm$(selection$)><"" then alp=1: goto L720
00590 L590: selclp=1
00600     goto L320
00610 ! _________________________________________
00620 L620: selclp=0
00630     goto ASK_NAS
00640 ! _________________________________________
00650 BACK: ! 
00660     bk1=bk1-1
00670     if bk1<1 then goto L620
00680     restore #file_num,key=fs_bk$(bk1)(1:alpha_key_length): nokey L620
00690     bk1=bk1-1
00700     goto L320
00710 ! _________________________________________
00720 L720: ! carry selection$ back; must pull key out in main program
00725     selection$=selection$(1:key_length)
00730 CANCEL_OUT: ! 
00740     close #win: 
00750     goto L770
00760 ! _________________________________________
00770 L770: fnend 
00780 ! ______________________________________________________________________
