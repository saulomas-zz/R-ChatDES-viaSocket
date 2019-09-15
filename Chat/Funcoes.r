source("Constantes.r")

permuteInput = function(data, initial) {
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

xorB = function(data1, data2) {
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

funcRowSbox = function(data) {
    return((data[1] * 2 ^ 1) + (data[6] * 2 ^ 0))
}

funcColSbox = function(data) {
    return((data[2] * 2 ^ 3) + (data[3] * 2 ^ 2) + (data[4] * 2 ^ 1) + (data[5] * 2 ^ 0))
}

newB = function(valorSBox) {
    tempB = numeric(4)

    resultadoDiv = 0
    countTemp = 4
    repeat {
        resultadoDiv = valorSBox %/% 2
        tempB[countTemp] = valorSBox %% 2

        if (resultadoDiv == 0) {
            break
        }


        valorSBox = resultadoDiv
        countTemp = countTemp - 1
    }

    return(tempB)
}

funcF = function(bMatrix) {
    tempF = numeric()

    for (i in 1:8) {
        rowSbox = funcRowSbox(bMatrix[i,])
        colSbox = funcColSbox(bMatrix[i,])

        valorSBox = sBox[[i]][[rowSbox + 1]][[colSbox + 1]]

        bN = newB(valorSBox)

        tempF = c(tempF, bN)
    }

    return(tempF)
}