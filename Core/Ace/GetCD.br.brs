00010 ! Replace S:\Core\Ace\GetCD.br
00020 ! ______________________________________________________________________
00030   def library fngetcd(&mcd$)
00040     ! ______________________________________________________________________
00050     library 'S:\Core\Library': fnshortpath$
00070     option retain 
00080     ! ______________________________________________________________________
00090     dim getcd_ln$*60
00100     dim oldcd$*60
00110     ! ______________________________________________________________________
00120     if oldcd$<>"" then mcd$=oldcd$ !:
            goto XIT
00130     ! ______________________________________________________________________
00135     oldcd$=mcd$=fnshortpath$("")
00230     goto XIT
00320     ! ______________________________________________________________________
00330 XIT: fnend 
00340 ! ______________________________________________________________________
