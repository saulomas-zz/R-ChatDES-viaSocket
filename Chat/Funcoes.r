source("Constantes.r")

## ---------------------------------------------------------- ##
## Função generica de Permutação                              ##
## ---------------------------------------------------------- ##

## ---------------------------------------------------------- ##
## Permuta uma tabela de bits de entrada "data" com a tabela  ##
## de permutação escolhida dentre as opções: initial, pc1,    ##
## expperm, pbox, final.                                      ##
## ---------------------------------------------------------- ##

permuteAll = function(data, opcao) {
    dataRef = NULL

    switch(opcao, 
        initial={
            dataRef = permIni
        }, pc1={
            dataRef = pc1 
        }, pc2={
            dataRef = pc2
        }, expperm={
            dataRef = expPerm
        }, pbox={
            dataRef = pBox  
        }, final={
            dataRef =  permFim
        }, {
            return = dataRef
        }
    )

    dataRefLen = length(dataRef)
    dataPerm = numeric(dataRefLen)

    for(i in 1:dataRefLen) {
        dataPerm[i] = data[dataRef[i]]
    }

    return(dataPerm)
}

## ---------------------------------------------------------- ## 
## Funções de Apoio                                           ##
## ---------------------------------------------------------- ##

## ---------------------------------------------------------- ##
## Divide um vetor "x" em "n" pedaços                         ##
## ---------------------------------------------------------- ##

chunk = function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

## ---------------------------------------------------------- ##
## Função para rotação em valores de 28 bits a partir das     ##
## subchaves de rotação                                       ##
## ---------------------------------------------------------- ##

shifter = function(data, round) {
    rows = 4
    cols = 7

    dataMatrix = matrix(data, rows, cols, byrow = TRUE)
    newMatrix = dataMatrix

    for(i in 1:shifts[round]) {
        posCol = c(1:4)
        col = c(newMatrix[1], newMatrix[2], newMatrix[3], newMatrix[4])

        #for(i in 1:shifts[round]) {
            for (k in 1:4) {
                if (posCol[k] + 1 > 4)
                    posCol[k] = 1
                else 
                    posCol[k] = posCol[k] + 1
            }
        #}

        col = col[c(posCol)] 

        newMatrix = cbind(newMatrix, col)
        tempMatrix = matrix(,rows,cols)
        start = rows + 1
        count = 1
        for (l in start:length(newMatrix)) {
            tempMatrix[count] = newMatrix[l]

            count = count + 1         
        }

        newMatrix = tempMatrix
    }

    return(as.vector(t(newMatrix)))
}

## ---------------------------------------------------------- ##
## Função Feistel e funções de apoio                          ##
## ---------------------------------------------------------- ##

xorBit = function(data1, data2) {
    if (length(data1) == length(data2)) {
        dataXOR = c(1:length(data1))

        for(i in 1:length(data1)) {
            if (xor(data1[i],data2[i])) {
                dataXOR[i] = 1
            } else {
                dataXOR[i] = 0
            }
        }

        return(dataXOR)
    }
}

getRowSbox = function(data) {
    return((data[1] * 2 ^ 1) + (data[6] * 2 ^ 0))
}

getColSbox = function(data) {
    return((data[2] * 2 ^ 3) + (data[3] * 2 ^ 2) + (data[4] * 2 ^ 1) + (data[5] * 2 ^ 0))
}

getNewBitsByValueSbox = function(valueSbox) {
    tempNewBits = numeric(4)

    resultDiv = 0
    countTemp = 4
    repeat {
        resultDiv = valueSbox %/% 2
        tempNewBits[countTemp] = valueSbox %% 2

        if (resultDiv == 0) {
            break
        }

        valueSbox = resultDiv
        countTemp = countTemp - 1
    }

    return(tempNewBits)
}

resultXORBySbox = function(resultXORmatrix) {
    tempResult = numeric()

    for (i in 1:8) {
        rowSbox = getRowSbox(resultXORmatrix[i,])
        colSbox = getColSbox(resultXORmatrix[i,])

        valueSbox = sBox[[i]][[rowSbox + 1]][[colSbox + 1]]

        bN = getNewBitsByValueSbox(valueSbox)

        tempResult = c(tempResult, bN)
    }

    return(tempResult)
}

feistel = function(rTemp, kI) {
    # expansão
    e_rTemp = permuteAll(rTemp, 'expperm')

    # mistura de chaves usando XOR 
    resultXOR = xorBit(e_rTemp, kI)
    resultXORmatrix = matrix(resultXOR, 8, 6, byrow = TRUE)

    # substituição usando as caixas de substituição "sbox"
    outSbox = resultXORBySbox(resultXORmatrix)

    # permutação usando "pbox"
    outSboxPerm = permuteAll(outSbox, 'pbox')

    return(outSboxPerm)
}

## ---------------------------------------------------------- ##
## Criptografia e Descriptografia                             ##
## ---------------------------------------------------------- ##

encodeDecode = function(msgBlock, key, encode = TRUE) {
    msgBlockPerm = permuteAll(msgBlock, 'initial')
    keyPerm = permuteAll(key, 'pc1')

## ---------------------------------------------------------- ##
## Geração das subchaves                                      ##
## ---------------------------------------------------------- ##

    c0 = chunk(keyPerm, 2)[[1]]
    d0 = chunk(keyPerm, 2)[[2]]

    cTemp = c0
    dTemp = d0

    cI = list()
    dI = list()

    for (i in 1:16) {
        c = shifter(cTemp, i)
        d = shifter(dTemp, i)

        cI = c(cI, list(c))
        dI = c(dI, list(d))

        cTemp = c
        dTemp = d
    }

## ---------------------------------------------------------- ##
## Geração das chaves finais permutando-as com a tabela "pc2" ##
## ---------------------------------------------------------- ##

    kI = list()
    for (i in 1:16) {
        k = permuteAll(c(cI[[i]],dI[[i]]), 'pc2')

        kI = c(kI, list(k))
    }

    rm(i)
    rm(c, cTemp, d, dTemp, k)

    l0 = chunk(msgBlockPerm, 2)[[1]]
    r0 = chunk(msgBlockPerm, 2)[[2]]

    lTemp = l0
    rTemp = r0

    lI = list()
    rI = list()

## ---------------------------------------------------------- ##
## Se "encode = TRUE" então deve ser Criptografado.           ##
## ---------------------------------------------------------- ##
    if (encode) {
        for(i in 1:16) {
            l = rTemp
            r = xorBit(lTemp, feistel(rTemp, kI[[i]]))

            lI = c(lI, list(l))
            rI = c(rI, list(r))
            
            rTemp = r
            lTemp = l
        }
## ---------------------------------------------------------- ##
## Se "encode = FALSE" então deve ser Descriptografado, logo  ##
## ordem de aplicação das subchaves deve ser invertidas       ##
## ---------------------------------------------------------- ##
    } else {
        for(i in 16:1) {
            l = rTemp
            r = xorBit(lTemp, feistel(rTemp, kI[[i]]))

            lI = c(lI, list(l))
            rI = c(rI, list(r))
            
            rTemp = r
            lTemp = l
        }
    }
## ---------------------------------------------------------- ##

    rm(i)
    rm(lTemp, rTemp, l, r)

## ---------------------------------------------------------- ##
## Inversão dos blocos finais                                 ##
## ---------------------------------------------------------- ##

    l16r16inverted = c(rI[[16]],lI[[16]])

## ---------------------------------------------------------- ##
## Permutação do bloco invertido com a tabela "permFim"       ##
## ---------------------------------------------------------- ##   

    l16r16invertedPerm = permuteAll(l16r16inverted, 'final')
    l16r16invertedPermRaw = as.raw(l16r16invertedPerm)
    ## View(matrix(l16r16invertedPerm, 8, 8, byrow = TRUE))

## ---------------------------------------------------------- ## 
## O retorno é diferenciado dependendo da ação.               ##
## Se é de Criptografia ou Descriptografia.                   ##
## ---------------------------------------------------------- ##

    if (encode) {
        return(packBits(l16r16invertedPermRaw, type = c("raw", "integer")))
    } else {
        return(rawToChar(packBits(l16r16invertedPermRaw, type = c("raw", "integer"))))
    }
}

textToNumericBits = function(text) {
    return(as.numeric(rawToBits(charToRaw(text))))
}

rawToNumericBits = function(textRaw) {
    return(as.numeric(rawToBits(textRaw)))
}

msgEncrypt = function(msg, key) {
    msgInNumBits = textToNumericBits(msg)
    keyInNumBits = textToNumericBits(key)

    while (length(msgInNumBits) %% 64 != 0) {
        msgInNumBits = c(msgInNumBits, numeric(1))
    }

    numBlocks = length(msgInNumBits) / 64
    blocksInMatrix = matrix(msgInNumBits, numBlocks, 64, byrow = TRUE)

    msgCiphered = NULL 
    for(i in 1:numBlocks) {
        msgCiphered = c(msgCiphered, encodeDecode(blocksInMatrix[i,], keyInNumBits, encode = TRUE))
    }

    return(msgCiphered)
}

msgDecrypt = function(msg, key) {
    msgInBitsNum = rawToNumericBits(msg)
    keyInNumBits = textToNumericBits(key)

    numBlocks = length(msgInBitsNum) / 64
    blocksInMatrix = matrix(msgInBitsNum, numBlocks, 64, byrow = TRUE)

    msgDeciphered = NULL 
    for(i in 1:numBlocks) {
        msgDeciphered = c(msgDeciphered, encodeDecode(blocksInMatrix[i,], keyInNumBits, encode = FALSE))
    }

    return(paste(msgDeciphered, collapse = ""))
}

## ---------------------------------------------------------- ##