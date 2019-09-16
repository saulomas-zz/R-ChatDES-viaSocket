source("Constantes.r")

## --------------------------------------------------------------------
## Funções de Permutação
## --------------------------------------------------------------------

permute = function(data, initial) {
    dataTemp = c(1:64)

    for(i in 1:64) {
        if (initial) {
            dataTemp[i] = data[permIni[i]]
        } else {
            dataTemp[i] = data[permFim[i]]
        }
    }

    return(dataTemp)
}

permuteChoice1 = function(data) {
    dataTemp = c(1:56)

    for(i in 1:56) {
        dataTemp[i] = data[pc1[i]]           
    }

    return(dataTemp)
}

permuteChoice2 = function(data) {
    dataTemp = c(1:48)

    for(i in 1:48) {
        dataTemp[i] = data[pc2[i]]           
    }

    return(dataTemp)
}

permuteDbox = function(data) {
    dataTemp = c(1:48)

    for(i in 1:48) {
        dataTemp[i] = data[dBox[i]]           
    }

    return(dataTemp)
}

permutePbox = function(data) {
    dataTemp = c(1:32)

    for(i in 1:32) {
        dataTemp[i] = data[pBox[i]]           
    }

    return(dataTemp)
}

## --------------------------------------------------------------------
## Funções de Apoio
## --------------------------------------------------------------------

chunk = function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

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

## --------------------------------------------------------------------
## Função-F
## --------------------------------------------------------------------

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

funcF = function(rTemp, kI) {
    e_rTemp = permuteDbox(rTemp)

    resultXOR = xorBit(e_rTemp, kI)
    resultXORmatrix = matrix(resultXOR, 8, 6, byrow = TRUE)

    outSbox = resultXORBySbox(resultXORmatrix)
    outSboxPerm = permutePbox(outSbox)

    return(outSboxPerm)
}

## --------------------------------------------------------------------
## Criptografia e Descriptografia
## --------------------------------------------------------------------

encodeDecode = function(inputTextInBitsNum, key, encode = TRUE) {
    inputTextInBitsNumPerm = permute(inputTextInBitsNum, initial = TRUE)

    inputKeyInBitsNum = keyInBitsNum(key)
    inputKeyInBitsNumPerm = permuteChoice1(inputKeyInBitsNum)

    c0 = chunk(inputKeyInBitsNumPerm, 2)[[1]]
    d0 = chunk(inputKeyInBitsNumPerm, 2)[[2]]

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

    kI = list()
    for (i in 1:16) {
        k = permuteChoice2(c(cI[[i]],dI[[i]]))

        kI = c(kI, list(k))
    }

    rm(i)
    rm(c, cTemp, d, dTemp, k)

    l0 = chunk(inputTextInBitsNumPerm, 2)[[1]]
    r0 = chunk(inputTextInBitsNumPerm, 2)[[2]]

    lTemp = l0
    rTemp = r0

    lI = list()
    rI = list()

    ## Se "encode = TRUE" então deve ser Criptografado.
    if (encode) {
        for(i in 1:16) {
            l = rTemp
            r = xorBit(lTemp, funcF(rTemp, kI[[i]]))

            lI = c(lI, list(l))
            rI = c(rI, list(r))
            
            rTemp = r
            lTemp = l
        }
    ## Se "encode = FALSE" então deve ser Descriptografado, logo a ordem de aplicação das subchaves deve ser invertidas
    } else {
        for(i in 16:1) {
            l = rTemp
            r = xorBit(lTemp, funcF(rTemp, kI[[i]]))

            lI = c(lI, list(l))
            rI = c(rI, list(r))
            
            rTemp = r
            lTemp = l
        }
    }

    rm(i)
    rm(lTemp, rTemp, l, r)

    l16r16inverted = c(rI[[16]],lI[[16]])
    l16r16invertedPerm = permute(l16r16inverted, initial = FALSE)
    l16r16invertedPermRaw = as.raw(l16r16invertedPerm)
    ## View(matrix(l16r16invertedPerm, 8, 8, byrow = TRUE))
    
    ## O retorno é diferenciado dependendo da ação. Se é de Criptografia ou Descriptografia.
    if (encode) {
        return(packBits(l16r16invertedPermRaw, type = c("raw", "integer")))
    } else {
        return(rawToChar(packBits(l16r16invertedPermRaw, type = c("raw", "integer"))))
    }
}

keyInBitsNum = function(key) {
    keyInRaw = charToRaw(key)

    while (length(keyInRaw) %% 8 != 0) {
        keyInRaw = c(keyInRaw, raw(1))
    }
    keyInBits = rawToBits(keyInRaw)

    return(as.numeric(keyInBits))
}

msgEncrypt = function(msg, key) {
    msgInRaw = charToRaw(msg)
    inputTextInRaw = msgInRaw

    while (length(inputTextInRaw) %% 16 != 0) {
        inputTextInRaw = c(inputTextInRaw, raw(1))
    }
    inputTextInBits = rawToBits(inputTextInRaw)
    inputTextInBitsNum = as.numeric(inputTextInBits)
    
    numBlocks = length(inputTextInBitsNum) / 64
    blocksInMatrix = matrix(inputTextInBitsNum, numBlocks, 64, byrow = TRUE)

    msgCiphered = NULL 
    for(i in 1:numBlocks) {
        msgCiphered = c(msgCiphered, encodeDecode(blocksInMatrix[i,], key, encode = TRUE))
    }

    return(msgCiphered)
}

msgDecrypt = function(msg, key) {
    msgInBits = rawToBits(msg)
    msgInBitsNum = as.numeric(msgInBits)
    
    numBlocks = length(msgInBitsNum) / 64
    blocksInMatrix = matrix(msgInBitsNum, numBlocks, 64, byrow = TRUE)

    msgDeciphered = NULL 
    for(i in 1:numBlocks) {
        msgDeciphered = c(msgDeciphered, encodeDecode(blocksInMatrix[i,], key, encode = FALSE))
    }

    return(paste(msgDeciphered, collapse = ""))
}