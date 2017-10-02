00010 ! Replace S:\Core\Print1099
00020 ! pr 1099 Forms (From a File) !:
        ! fnAdd1099 - adds a 1099 to be printed/exported !:
        ! fnPrint1099 - Prints all added 1099s !:
        ! Chain to this program - does the same as fnPrint1099
00030 ! _______________________________________________________________________
00040 ! _______________________________________________________________________
00050 ! ______________________________________________________________________
00060 ! this is the part of the program that tells it what to do when you !:
        ! chain to i - or just load and run it.
00070   library 'S:\Core\Library': fnprint1099,fnxit,fntop
00080   dim cap$*128
00090   let fntop("S:\Core\Print1099",cap$="Print 1099s")
00100   let fnprint1099(lz1$)
00110   let fnxit
00120 ! _______________________________________________________________________
00130 ! _______________________________________________________________________
00140 ! _______________________________________________________________________
00150   def library fnadd1099(mat cinfo$, mat einfo$, mat box)
00160     dim oldbox(22)
00170     library 'S:\Core\Library': fngethandle,fnpause
00180 ! ___________________________
00181     if einfo$(1)='' then let fnpause
00190     open #dave=fngethandle: 'Name=1099-'&session$&'.dat,RecL=810,Use,Version=1,KFName=S:\Core\Data\Print1099.Idx,KPs=241,KLn=40',internal,outin,keyed 
00200     read #dave,using 'Form Pos 481,22*N 15.2',key=rpad$(einfo$(1),40),reserve: mat oldbox nokey DAVENOKEY
00210     mat box=box+oldbox
00220     rewrite #dave,using 'Form Pos 481,22*N 15.2',same: mat box
00221     pr 'fnAdd1099 - REWritting for '&einfo$(1) ! XXX
00230     goto ADD1099DONE
00240 ! ___________________________
00250 DAVENOKEY: ! !:
          write #dave,using 'Form Pos 1,6*C 40,6*C 40,22*N 15.2': mat cinfo$,mat einfo$,mat box
00251     pr 'fnAdd1099 - Writting for '&einfo$(1) ! XXX
00260     goto ADD1099DONE
00270 ! ___________________________
00280 ADD1099DONE: close #dave: 
00290 ! reset all variables (used in this function) !:
          mat einfo$=('') : mat box=(0) : let dave=0 : mat oldbox=(0) !:
          ! don't reset Mat cInfo$, because it's annoying to loose these values
00300   fnend 
00310 ! _______________________________________________________________________
00320 ! _______________________________________________________________________
00330 ! _______________________________________________________________________
00340   def library fnprint1099(; lz1$)
00350     library 'S:\Core\Library': fntop,fnopenprn,fncloseprn,fncno,fnerror,fntop,fnxit,fncursys$,fngethandle
00360     on error goto PRINT1099ERTN
00370 ! ___________________________
00380     dim cinfo$(6)*40,einfo$(6)*40,box(22)
00390 ! cInfo$(1)*40 ! Company Name
00400 ! cInfo$(2)*40 ! Company Address (1)
00410 ! cInfo$(3)*40 ! Company Address (2)
00420 ! cInfo$(4)*40 ! Company City State Zip
00430 ! cInfo$(5)*12 ! Payer/Company Phone Number
00440 ! cInfo$(6)*12 ! Federal Identification Number
00450 ! LZ1$*1 ! 'D', 'L', or 'E' -[D]ot Matrix, [L]aser, [E]xport
00460 ! ___________________________
00470     if lz1$='' then let lz1$='D'
00480 ! fnwait !:
          on fkey 5 goto EODAVE
00490     open #dave=fngethandle: 'Name=1099-'&session$&'.dat',internal,outin ioerr XIT
00500     if lz1$='E' then !:
            open #exportfile=fngethandle: 'Name=\1099Etc.Wrk\W2Data\1099Dat.Prn,Replace',display,output else !:
            let fnopenprn
00510 READ_DAVE: ! 
00520     read #dave, using 'Form Pos 1,12*C 40,22*N 15.2': mat cinfo$,mat einfo$,mat box eof EODAVE
00530     if lz1$='E' then gosub EXPORT1099 else !:
            if lz1$='L' then gosub PRINT1099LASER else !:
              if lz1$='D' then gosub PRINT1099DOT
00540     goto READ_DAVE
00550 ! ___________________________
00560 EODAVE: ! 
00570     if lz1$='E' then !:
            close #exportfile: else !:
            let fncloseprn !:
            pr 'closing prn' ! XXX
00571     pr str$(lrec(dave))&' 1099s should have been printed.'
00580     close #dave,free: 
00590     goto XIT
00600 ! ______________________________________________________________________
00610 PRINT1099DOT: ! Dot Matrix 1099
00620     pr #255: '' !:
          pr #255: '' !:
          pr #255: '' !:
          pr #255: ''
00630     pr #255,using 'Form POS 7,C 30,N 11.2': cinfo$(1)(1:30),box(1)
00640     pr #255,using 'Form POS 7,C 30': cinfo$(2)(1:30)
00650     pr #255,using 'Form POS 7,C 30': cinfo$(4)(1:30)
00660     pr #255,using 'Form POS 37,N 11.2': box(2)
00670     pr #255: '' !:
          pr #255: ''
00680     pr #255,using 'Form POS 24,C 12,POS 37,N 11.2,N 13.2': cinfo$(5)(1:12),box(3),box(4)
00690     pr #255: '' !:
          pr #255: ''
00700     pr #255,using 'Form POS 7,2*C 15,N 11.2,N 13.2': cinfo$(6)(1:15),einfo$(1)(1:15),box(5),box(6)
00710     pr #255: '' !:
          pr #255: '' !:
          pr #255,using 'Form POS 7,C 30': einfo$(2)(1:30) !:
          pr #255: ''
00720     pr #255,using 'Form POS 7,N 11.2,N 13.2': box(7),box(8)
00740     pr #255: ''
00750     pr #255,using 'Form POS 7,C 32': einfo$(3)(1:32)
00760     pr #255: '' !:
          pr #255: ''
00770     pr #255,using 'Form POS 7,C 32': einfo$(5)(1:32)
00780     pr #255: '' !:
          pr #255: ''
00790     pr #255,using 'Form POS 7,C 8': einfo$(6)(1:8)
00800     for j=1 to 6 !:
            pr #255: '' !:
          next j
00810     return 
00820 ! ______________________________________________________________________
00830 PRINT1099LASER: ! Laser 1099
00840     pr #255: ''
00850     pr #255,using 'Form POS 7,C 30,N 11.2': cinfo$(1)(1:30),box(1)
00860     pr #255,using 'Form POS 7,C 30': cinfo$(2)(1:30)
00870     pr #255,using 'Form POS 7,C 30': cinfo$(4)(1:30)
00880     pr #255,using 'Form POS 37,N 11.2': box(2)
00890     pr #255: '' !:
          pr #255: ''
00900     pr #255,using 'Form POS 24,C 12,POS 37,N 11.2,N 13.2': cinfo$(5)(1:12),box(3),box(4)
00910     pr #255: '' !:
          pr #255: ''
00920     pr #255,using 'Form POS 7,2*C 15,N 11.2,N 13.2': cinfo$(6)(1:15),einfo$(1)(1:15),box(5),box(6)
00930     pr #255: '' !:
          pr #255: '' !:
          pr #255: ''
00940     pr #255,using 'Form POS 7,C 30,N 11.2,N 13.2': einfo$(2)(1:30),box(7),box(8)
00950     pr #255: '' !:
          pr #255: ''
00960     pr #255,using 'Form POS 7,C 32': einfo$(3)(1:32)
00970     pr #255: '' !:
          pr #255: ''
00980     pr #255,using 'Form POS 7,C 32': einfo$(5)(1:32)
00990     pr #255: '' !:
          pr #255: ''
01000     pr #255,using 'Form POS 7,C 32': einfo$(6)(1:32)
01010     let x+=1 : if x=2 then !:
            pr #255: newpage : let x=0 else !:
            for j=1 to 10 : pr #255: '' : next j
01020     return 
01030 ! ______________________________________________________________________
01040 EXPORT1099: ! 
01050     pr #exportfile: "01 ";" "
01060     pr #exportfile: "02 ";cinfo$(5)
01070     pr #exportfile: "03 ";cinfo$(1)
01080     pr #exportfile: "04 ";box(1)
01090     pr #exportfile: "05 ";" "
01100     pr #exportfile: "06 ";cinfo$(2)
01110     pr #exportfile: "07 ";box(2)
01120     pr #exportfile: "08 ";cinfo$(4)
01130     pr #exportfile: "09 ";box(3)
01140     pr #exportfile: "10 ";box(4)
01150     pr #exportfile: "11 ";b$(1)
01160     pr #exportfile: "12 ";einfo$(1)
01170     pr #exportfile: "13 ";box(5)
01180     pr #exportfile: "14 ";box(6)
01190     pr #exportfile: "15 ";box(7)
01200     pr #exportfile: "16 ";box(8)
01210     pr #exportfile: "17 ";einfo$(2)
01220     pr #exportfile: "18 ";" "
01230     pr #exportfile: "19 ";" "
01240     pr #exportfile: "20 ";box(10)
01250     pr #exportfile: "21 ";ad$(1)
01260     pr #exportfile: "22 ";ad$(2)
01270     pr #exportfile: "23 ";" "
01280     pr #exportfile: "24 ";0
01290     pr #exportfile: "25 ";einfo$(6)
01300     pr #exportfile: "26 ";" "
01310     pr #exportfile: "27 ";0
01320     pr #exportfile: "28 ";" "
01330     pr #exportfile: "29 ";0
01340     pr #exportfile: "30 ";" "
01350 ! pr #exportfile: "31 ";" "
01360 ! pr #exportfile: "32 ";0
01370     pr #exportfile: "*"
01380     return 
01390 ! ______________________________________________________________________
01400 PRINT1099ERTN: let fnerror(program$,err,line,act$,"xit")
01410     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01420     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01430     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01440 ERTN_EXEC_ACT: execute act$ : goto PRINT1099ERTN
01450 ! ______________________________________________________________________
01460 XIT: ! 
01470   fnend 
01480 ! ______________________________________________________________________
01490 ! ______________________________________________________________________
01500 ! ______________________________________________________________________
