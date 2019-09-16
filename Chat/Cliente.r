source("EncodeDecode_byDES.r")
#O arquivo acima localizado na mesma pasta desse arquivo contém as funções:
#=> De geração de chave (getChaveP, getChaveQ, getChaveN, getChaveNFi, getChaveE, getChaveD)
#=> De criptografia e descriptografia (toMsgCrypt, toMsgDecrypt)

client <- function() {
    #--------------------------------------------------------------------
    #Geração das Chaves Públicas e Privadas    

    # repeat {
    #     f <- file("stdin")
    #     open(f)
    #     writeLines("Digite uma chave de 8 caracteres que será usada para Criptografia e Decriptografia", sep=": ")
    #     key <- readLines(f, n=1)
    #     if(tolower(key)=="q"){
    #         break
    #     }

    #     if (nchar(key) == 8) {
    #         break
    #     }

    #     print("A chave deve conter 8 caracteres!")
    # }

    key = "saulomas"
    
    #--------------------------------------------------------------------
    print("Enviando Chave digitada para o Servidor")
    con <- socketConnection(host="localhost", port=666, blocking=TRUE, server=FALSE, open="r+")

    # enviar a chave para o Servidor
    write_resp = writeLines(key, con)

    close(con)
    #--------------------------------------------------------------------

    print("Chat Aberto!!!")
    while(TRUE){
        con = socketConnection(host="localhost", port = 777, blocking=TRUE, server=FALSE, open="r+")

        # cliente captura mensagem da entrada padrao (teclado)
        fChat <- file("stdin")
        open(fChat)
        writeLines("Mensagem", sep=": ")
        msg <- readLines(fChat, n=1)
        if(tolower(msg)=="q"){
            break
        }

        # cliente criptografa a mensagem e a envia para o servidor
        msgEncrypted = msgEncrypt(msg, key)
        msgEncryptedInChar = rawToChar(msgEncrypted)

        write_resp = writeLines(msgEncryptedInChar, con)

        # cliente recebe mensagem enviada pelo servidor
        msgEncrypted = readLines(con, 1)
        msgEncryptedInRaw = charToRaw(msgEncrypted)

        # cliente decriptografa a mensagem e a mostra na tela
        # fazer aqui a decriptografia
        msg = msgDecrypt(msgEncryptedInRaw, key)
        print(msg)

        close(con)    
    }
}
client()