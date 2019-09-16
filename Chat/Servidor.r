source("Funcoes.r")

## --------------------------------------------------------------------
## O arquivo acima localizado na mesma pasta desse arquivo contém as 
##            funções necessárias para os cálculos de criptografia e 
##                                  descriptografia do algoritmo DES
## --------------------------------------------------------------------

server <- function() {  
    ## --------------------------------------------------------------------
    print("Aguardando Chave digitada do Cliente")

    con <- socketConnection(host="localhost", port=666, blocking=TRUE, server=TRUE, open="r+")

    ## receber a chave publica do cliente
    key = readLines(con,1)
    print(paste("Chave:", key))

    close(con)
    ## --------------------------------------------------------------------

    while(TRUE){
        writeLines("Escutando...")
        con = socketConnection(host="localhost", port=777, blocking=TRUE, server=TRUE, open="r+")

        ## Servidor recebe mensagem enviada do Cliente
        msgEncrypted = readLines(con, 1)
        msgEncryptedInRaw = charToRaw(msgEncrypted)

        ## Servidor decriptografa a mensagem e a mostra na tela
        msg = msgDecrypt(msgEncryptedInRaw, key)
        print(msg)
        
        ## Servidor captura mensagem da entrada padrao via Teclado
        f = file("stdin")
        open(f)
        writeLines("Digite sua mensagem", sep=": ")
        msg <- readLines(f, n=1)
        if(tolower(msg)=="q"){
            break
        }
        
        ## Servidor criptografa a mensagem e a envia para o Cliente
        msgEncrypted = msgEncrypt(msg, key)
        msgEncryptedInChar = rawToChar(msgEncrypted)
        write_resp <- writeLines(msgEncryptedInChar, con)

        close(con)
    }
}
server()