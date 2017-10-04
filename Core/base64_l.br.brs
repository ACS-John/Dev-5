00001  !                "  BASE64_L  "
00002  !        Libary To Encode And Decode Data In Base64
00003  !    Last Date 12/05/16 Shk
00004  !
00100  ! #Autonumber# 100,20
00120     on error goto ERR_TRAP
00140     option base 0
00160     pr "Library Base64_L" : end
00180     def library Fn_Encodebase64(&Content$)
00200        if Trim$(Content$)="" then goto RT_ENCODE
00220        gosub RD_ROUTINE
00240        dim Bl$*1200000,Line$*1200000,B64$*1200000
00260        encode=1
00280        line$=Content$
00300        l=Len(Line$)
00320        for I=1 to L
00340           bl$=Bl$&Bt$(Ord(Line$(I:I)))
00360        next I
00380        l=Len(Bl$): let Nullcount=Mod(L,3)
00400        if Nullcount=2 then l=L+16
00420        if Nullcount=1 then l=L+8
00440        bl$=Rpad$(Bl$,L,"0")
00460        l=Len(Bl$)
00480        let D=Int(L/6): e=D*6
00500        b64=0: for I=1 to E step 6
00520           let F$=Bl$(I:I+5)
00540           if F$(1:1)="1" then b64=B64+32
00560           if F$(2:2)="1" then b64=B64+16
00580           if F$(3:3)="1" then b64=B64+8
00600           if F$(4:4)="1" then b64=B64+4
00620           if F$(5:5)="1" then b64=B64+2
00640           if F$(6:6)="1" then b64=B64+1
00660           b64$=B64$&B64t$(B64) : b64=0
00680        next I
00700        let X=Len(B64$)
00720        if Nullcount=2 then b64$=B64$(1:X-2)&"==" : goto RT_ENCODE
00740        if Nullcount=1 then b64$=B64$(1:X-1)&"="
00760  RT_ENCODE: content$=B64$
00780     fnend
00800     def library Fn_Decodebase64(&Content$)
00820        if Trim$(Content$)="" then goto RT_DECODE
00840        dim Bl$*1200000,Line$*1200000,B64$*1200000,Id(1200000)
00860        let Decode=1
00880        gosub RD_ROUTINE
00900        line$=Content$
00920        l=Len(Line$)
00940        for I=1 to L
00960           for Q=0 to 63
00980              if Line$(I:I)=B64t$(Q) then let Id(I)=Q
01000           next Q
01020        next I
01040        for I=1 to L
01060           sav=Id(I)
01080           if Sav<32 then bt$="0" else sav=Sav-32 : bt$="1"
01100           if Sav<16 then bt$=Bt$&"0" else sav=Sav-16: bt$=Bt$&"1"
01120           if Sav<8 then bt$=Bt$&"0" else sav=Sav-8 : bt$=Bt$&"1"
01140           if Sav<4 then bt$=Bt$&"0" else sav=Sav-4 : bt$=Bt$&"1"
01160           if Sav<2 then bt$=Bt$&"0" else sav=Sav-2 : bt$=Bt$&"1"
01180           if Sav<1 then bt$=Bt$&"0" else sav=Sav-1 : bt$=Bt$&"1"
01200           if Sav<>0 then pr "Problem" : pause
01220           bl$=Bl$&Bt$
01240        next I
01260        l=Len(Bl$)
01280        let D=Int(L/8): e=D*8
01300        line$="" : for I=1 to E step 8
01320           for Q=0 to 255
01340              let F$=Bl$(I:I+7)
01360              if F$=Bt$(Q) then line$=Line$&Chr$(Q) : let Q=255
01380           next Q
01400        next I
01420        let X=Len(Line$) : if Line$(X-1:X)=Chr$(0)&Chr$(0) then line$=Line$(1:X-2)
01440        if Line$(X:X)=Chr$(0) then line$=Line$(1:X-1)
01460  RT_DECODE: content$=Trim$(Line$)
01480     fnend
01500  RD_ROUTINE: !
01520     dim Bt$(255)*8,B64t$(64)*1
01540     for Dec=0 to 255
01560        read Bt$(Dec)
01580     next Dec
01600     for Dec=0 to 63
01620        read B64t$(Dec)
01640     next Dec
01660     return
01680  !
01700  !  Binary Table
01720     data "00000000","00000001","00000010","00000011","00000100","00000101","00000110","00000111","00001000"
01740     data "00001001","00001010","00001011","00001100","00001101","00001110","00001111","00010000","00010001"
01760     data "00010010","00010011","00010100","00010101","00010110","00010111","00011000","00011001","00011010"
01780     data "00011011","00011100","00011101","00011110","00011111","00100000","00100001","00100010","00100011"
01800     data "00100100","00100101","00100110","00100111","00101000","00101001","00101010","00101011","00101100"
01820     data "00101101","00101110","00101111","00110000","00110001","00110010","00110011","00110100","00110101"
01840     data "00110110","00110111","00111000","00111001","00111010","00111011","00111100","00111101","00111110"
01860     data "00111111","01000000","01000001","01000010","01000011","01000100","01000101","01000110","01000111"
01880     data "01001000","01001001","01001010","01001011","01001100","01001101","01001110","01001111","01010000"
01900     data "01010001","01010010","01010011","01010100","01010101","01010110","01010111","01011000","01011001"
01920     data "01011010","01011011","01011100","01011101","01011110","01011111","01100000","01100001","01100010"
01940     data "01100011","01100100","01100101","01100110","01100111","01101000","01101001","01101010","01101011"
01960     data "01101100","01101101","01101110","01101111","01110000","01110001","01110010","01110011","01110100"
01980     data "01110101","01110110","01110111","01111000","01111001","01111010","01111011","01111100","01111101"
02000     data "01111110","01111111","10000000","10000001","10000010","10000011","10000100","10000101","10000110"
02020     data "10000111","10001000","10001001","10001010","10001011","10001100","10001101","10001110","10001111"
02040     data "10010000","10010001","10010010","10010011","10010100","10010101","10010110","10010111","10011000"
02060     data "10011001","10011010","10011011","10011100","10011101","10011110","10011111","10100000","10100001"
02080     data "10100010","10100011","10100100","10100101","10100110","10100111","10101000","10101001","10101010"
02100     data "10101011","10101100","10101101","10101110","10101111","10110000","10110001","10110010","10110011"
02120     data "10110100","10110101","10110110","10110111","10111000","10111001","10111010","10111011","10111100"
02140     data "10111101","10111110","10111111","11000000","11000001","11000010","11000011","11000100","11000101"
02160     data "11000110","11000111","11001000","11001001","11001010","11001011","11001100","11001101","11001110"
02180     data "11001111","11010000","11010001","11010010","11010011","11010100","11010101","11010110","11010111"
02200     data "11011000","11011001","11011010","11011011","11011100","11011101","11011110","11011111","11100000"
02220     data "11100001","11100010","11100011","11100100","11100101","11100110","11100111","11101000","11101001"
02240     data "11101010","11101011","11101100","11101101","11101110","11101111","11110000","11110001","11110010"
02260     data "11110011","11110100","11110101","11110110","11110111","11111000","11111001","11111010","11111011"
02280     data "11111100","11111101","11111110","11111111"
02300  ! Base 64 Table
02320     data "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"
02340     data "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"
02360     data "0","1","2","3","4","5","6","7","8","9","+","/"
02380  !
90000  ! #Autonumber# 90000,10
90010  ERR_TRAP: !
90020  CTRL_C: !
90030     content$="ERR"&Str$(Err)
90040     if Decode=1 then goto RT_DECODE else goto RT_ENCODE
