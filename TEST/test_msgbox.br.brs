00040   library 'S:\Core\Library': fnmsgbox
00080   dim ml$(10)*80 ! message box message lines
03770   mat ml$(1) !:
        let ml$(1)="You already have a transaction with reference # 11092001." !:
        let fnmsgbox(mat ml$,resp$,cap$,0)
