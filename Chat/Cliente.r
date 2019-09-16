source("Funcoes.r")

## --------------------------------------------------------------------
## O arquivo acima localizado na mesma pasta desse arquivo contém as 
##            funções necessárias para os cálculos de criptografia e 
##                                  descriptografia do algoritmo DES
## --------------------------------------------------------------------

client <- function() {
    ## --------------------------------------------------------------------
    ## --------------------------------------------------------------------
    ## Chave que será usada para Encriptar e Decriptar a troca de Mensagens 
    ## --------------------------------------------------------------------
    repeat {
        f <- file("stdin")
        open(f)
        writeLines("Digite uma chave de 8 caracteres que será usada para Criptografia e Decriptografia", sep=": ")
        key <- readLines(f, n=1)
        if(tolower(key)=="q"){
            break
        }

        if (nchar(key) == 8) {
            break
        }

        print("A chave deve conter 8 caracteres!")
    }

    ##--------------------------------------------------------------------

    ## Caso queira usar uma chave fixa, descomente a linha abaixo e comente 
    ## todo o bloco "repeat" acima

    #key = "saulomas" 
    
    ## --------------------------------------------------------------------
    ## --------------------------------------------------------------------

    print("Enviando Chave digitada para o Servidor")
    con <- socketConnection(host="localhost", port=666, blocking=TRUE, server=FALSE, open="r+")

    # Envio da chave para o Servidor
    write_resp = writeLines(key, con)

    close(con)

    ## --------------------------------------------------------------------
    ## --------------------------------------------------------------------
    ## Início do Chat 
    ## --------------------------------------------------------------------    
    print("Chat Aberto!!!")
    while(TRUE){
        con = socketConnection(host="localhost", port=777, blocking=TRUE, server=FALSE, open="r+")

        ## Cliente captura mensagem da entrada padrao via Teclado
        fChat <- file("stdin")
        open(fChat)
        writeLines("Digite sua mensagem", sep=": ")
        msg <- readLines(fChat, n=1)
        if(tolower(msg)=="q"){
            break
        }

        ## Cliente criptografa a mensagem e a envia para o Servidor
        msgEncrypted = msgEncrypt(msg, key)
        msgEncryptedInChar = rawToChar(msgEncrypted)
        write_resp = writeLines(msgEncryptedInChar, con)

        ## Cliente recebe mensagem enviada do Servidor
        msgEncrypted = readLines(con, 1)
        msgEncryptedInRaw = charToRaw(msgEncrypted)

        ## Cliente decriptografa a mensagem e a mostra na tela
        msg = msgDecrypt(msgEncryptedInRaw, key)
        print(msg)

        close(con)    
    }
    ## --------------------------------------------------------------------
    ## Fim do Chat
    ## --------------------------------------------------------------------
    ## --------------------------------------------------------------------
}
client()