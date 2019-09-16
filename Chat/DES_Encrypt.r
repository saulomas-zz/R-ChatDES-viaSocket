#install.packages('chunk2', repos='http://cran.us.r-project.org')

#rawToChar(packBits(rawToBits(charToRaw("64bitKey")), type = c("raw", "integer")))

source("Funcoes.r")

msgEncrypt = function(msg) {
    msgInRaw = charToRaw(msg)

    while (length(msgInRaw) %% 16 != 0) {
        inputTextInRaw = c(inputTextInRaw, raw(1))
    }
    inputTextInBits = rawToBits(inputTextInRaw)
    inputTextInBitsNum = as.numeric(inputTextInBits)
    
    numBlocks = length(inputTextInBitsNum) / 64
    blocksInMatrix = matrix(inputTextInBitsNum, numBlocks, 64, byrow = TRUE)

    finalcipher = NULL 
    for(i in 1:numBlocks) {
        finalcipher = c(finalcipher, enCryptBlock(blocksInMatrix[i,]))
    }
}

# inputText = "Secret Message"
# inputTextInRaw = charToRaw(inputText)

# while (length(inputTextInRaw) %% 16 != 0) {
#     inputTextInRaw = c(inputTextInRaw, raw(1))
# }

# inputTextInBits = rawToBits(inputTextInRaw)
# inputTextInBitsNum = as.numeric(inputTextInBits)

# inputTextInBitsNum = c(0,1,0,1,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,0,1,1,1,0,0,1,0,0,1,1,0,0,1,0,1,0,1,1,1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,1,0,1)
# inputTextInBitsNumPerm = permute(inputTextInBitsNum, initial = TRUE)

enCryptBlock() = function(inputTextInBitsNum) {
    inputTextInBitsNumPerm = permute(inputTextInBitsNum, initial = TRUE)

    # inputKey = "64bitKey"
    # inputKeyInRaw = charToRaw(inputKey)
    # inputKeyInBits = rawToBits(inputKeyInRaw)
    # inputKeyInBitsNum = as.numeric(inputKeyInBits)

    inputKeyInBitsNum = c(0,0,1,1,0,1,1,0,0,0,1,1,0,1,0,0,0,1,1,0,0,0,1,0,0,1,1,0,1,0,0,1,0,1,1,1,0,1,0,0,0,1,0,0,1,0,1,1,0,1,1,0,0,1,0,1,0,1,1,1,1,0,0,1)
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

    for(i in 1:16) {
        l = rTemp
        #r = lTemp

        e_rTemp = permuteDbox(rTemp)    
        resultXOR = xorBit(e_rTemp, kI[[i]])

        resultXORmatrix = matrix(resultXOR, 8, 6, byrow = TRUE)

        outSbox = resultXORBySbox(resultXORmatrix)
        outSboxPerm = permutePbox(outSbox)

        r = xorBit(lTemp, outSboxPerm)

        lI = c(lI, list(l))
        rI = c(rI, list(r))
        
        rTemp = r
        lTemp = l
    }

    rm(i)
    rm(lTemp, rTemp, l, r, e_rTemp, resultXOR, resultXORmatrix, outSbox, outSboxPerm)

    l16r16inverted = c(rI[[16]],lI[[16]])

    l16r16invertedPerm = permute(l16r16inverted, initial = FALSE)

    l16r16invertedPermRaw = as.raw(l16r16invertedPerm)

    return(packBits(l16r16invertedPermRaw, type = c("raw", "integer")))
}


# View(matrix(l16r16invertedPerm, 8, 8, byrow = TRUE))

# M = 0123456789ABCDEF
# inBlock = c(0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,1,1,1,0,0,0,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1)

# K = 133457799BBCDFF1
# keyK = c(0,0,0,1,0,0,1,1,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,1,0,1,1,1,1,0,0,1,1,0,0,1,1,0,1,1,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,1,1,1,1,1,0,0,0,1)

#rawToChar(packBits(rawToBits(charToRaw("0123456789ABCDEF")), type = c("raw", "integer")))