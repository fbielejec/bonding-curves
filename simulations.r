###

## cw = connector_token_balance / continuous_token_total_value
## continuous_token_total_value = continuous_token_price * continuous_token_supply
## continuous_token_price = connector_token_balance / continuous_token_supply * cw

## --- functions --- #

calculate_purchase_return <- function (continuous_token_supply,
                                       connector_token_balance,
                                       connector_weight,
                                       deposit_amount) {
    ## returns the amount of continuous token you get for depositing deposit_amount of connector token
    purchase_return = continuous_token_supply * ((1 + deposit_amount / connector_token_balance) ^ (connector_weight) - 1)
    list(return = purchase_return, price = deposit_amount / purchase_return)
}

calculate_sale_return <- function (continuous_token_supply,
                                   connector_token_balance,
                                   connector_weight,
                                   sell_amount) {
    ## returns the amount of connector token you get for selling sell_amount of continuous token
    sale_return = connector_token_balance * (1 - (1 - sell_amount / continuous_token_supply) ^ (1 / connector_weight))
    list(return = sale_return, price = sale_return / sell_amount)
}

calculate_sell_amount <- function (continuous_token_supply,
                                   connector_token_balance,
                                   connector_weight,
                                   sale_return) {
    ## returns the amount of continuous token that needs to be sold in order to receive back sale_return amount of connector tokens
    sell_amount = (1 - (1 - sale_return / connector_token_balance) ^ (connector_weight)) / continuous_token_supply
    list(return = sell_amount, price = sale_return / sell_amount)
}

## --- parameters ---#

## cw (connector_weight) parameter controls the curve shape (the price elasticity)
## cw = 1 results in a flat (constant) curve
## cw = 0.5 in a linear curve
## cw = 0.1 in an exponential curve
## cw = 0.9 in an logarithmic curve

cw                           <- .5
init_continuous_token_supply <- 1000
init_continuous_token_price  <- 1

## --- simulations ---#

N    <- 1000
data <- list ()

## first transaction cannot buy more than the initial supply
init_deposit                 <- floor( runif(1, 1, init_continuous_token_price * init_continuous_token_supply) )
data$continuous_token_supply <- c (init_continuous_token_supply )
data$continuous_token_price  <- c (init_continuous_token_price)
data$connector_token_balance <- c (init_deposit)
data$tx                      <- c("buy")

#############################################
## --- simulation : random transactions ---##
#############################################

for (i in 2 : N) {

    ## is_deposit <- TRUE
    is_deposit <- sample(size = 1, c(TRUE, FALSE), prob = c(0.5, 0.5))

    if (is_deposit == TRUE) {

        connector_token_deposit_amount <- floor( runif(1, 1, 200) )

        purchase_return <- calculate_purchase_return (data$continuous_token_supply [i - 1],
                                                      data$connector_token_balance [i - 1],
                                                      cw,
                                                      connector_token_deposit_amount)

        data$connector_token_balance <- c (data$connector_token_balance, data$connector_token_balance [i - 1] + connector_token_deposit_amount)
        data$continuous_token_supply <- c (data$continuous_token_supply, data$continuous_token_supply [i - 1] + purchase_return$return)
        data$continuous_token_price  <- c (data$continuous_token_price, purchase_return$price)
        data$tx                      <- c(data$tx, "buy")

    } else {

        continuous_token_sell_amount <- min(data$continuous_token_supply [i - 1] - 1, floor( runif(1, 1, 200 ) ))

        sale_return <- calculate_sale_return (data$continuous_token_supply [i - 1],
                                              data$connector_token_balance [i - 1],
                                              cw,
                                              continuous_token_sell_amount)

        data$connector_token_balance <- c (data$connector_token_balance, data$connector_token_balance [i - 1] - sale_return$return)
        ## sold tokens are burned
        data$continuous_token_supply <- c (data$continuous_token_supply, data$continuous_token_supply [i - 1] - continuous_token_sell_amount)
        data$continuous_token_price  <- c (data$continuous_token_price, purchase_return$price)
        data$tx                      <- c(data$tx, "sell")
    }

}

## -- plots -- ##

##png(file="plot.png")

data <- as.data.frame(data)
## data <- data[ which(data$tx=='buy'), ]

plot(
    x = data$continuous_token_supply,
    y = data$continuous_token_price,
    pch = 19,
    col = data$tx,
    xlab = "Supply",
    ylab = "Price"
)

legend('topleft', legend = levels(data$tx), col = 1:2, cex = 1.8, pch = 19)

## price = m * supply ^n
n = (1 / cw) - 1
sortedData = data[order(data$continuous_token_supply),]
linear_model <- lm(continuous_token_price ~ continuous_token_supply + I(continuous_token_supply^n), data = sortedData)
lines(sortedData$continuous_token_supply, predict(linear_model), lwd = 2, col = "blue")

##dev.off()

##########################################
## --- simulation : x % tokens burnt ---##
##########################################

## --- bonding curve parameters
## cw = 1 constant curve
## cw = 0.5 in a linear curve
## cw = 0.1 in an exponential curve
## cw = 0.9 in an logarithmic curve

cw                           <- .1 #.1
init_continuous_token_supply <- 1000
init_continuous_token_price  <- .01

## --- return parameters
## NOTE: on a system scale these parameters need to be tuned such that we are ensured never to run out of tokens to sell to new users (assuming we premine a fixed pool of tokens)
## we need to always have a reserve of tokens to keep in order to satisfy the demand

## % of a video token balance in circulation that is destroyed and it’s value returned to the users (with a commission paid to the creator and entity)
returns_parameter  <- 0.01
creator_commission <- 0.1
operator_commission   <- 0.05

## -- simulation

N        <- 1000
data     <- list ()
earnings <- list ()
earnings$creator <- 0
earnings$operator   <- 0

init_deposit                 <- floor( runif(1, 1, 200) ) # 10
init_return                  <- init_continuous_token_price * init_deposit
data$continuous_token_supply <- c (init_continuous_token_supply + init_return)
data$continuous_token_price  <- c (init_continuous_token_price)
data$connector_token_balance <- c (init_deposit)

earnings$first_investor <- list(holdings = init_return,
                                earnings = 0)

for (i in 2 : N) {

    connector_token_deposit_amount <- floor( runif(1, 1, 200) ) #2000

    purchase_return <- calculate_purchase_return (data$continuous_token_supply [i - 1],
                                                  data$connector_token_balance [i - 1],
                                                  cw,
                                                  connector_token_deposit_amount)

    data$connector_token_balance <- c (data$connector_token_balance, data$connector_token_balance [i - 1] + connector_token_deposit_amount)
    data$continuous_token_supply <- c (data$continuous_token_supply, data$continuous_token_supply [i - 1] + purchase_return$return)
    data$continuous_token_price  <- c (data$continuous_token_price, purchase_return$price)

    ## arbitrary probabilities
    is_returns <- sample(size = 1, c(TRUE, FALSE), prob = c(0.5, 0.5)) #0.01
    if(is_returns == TRUE) {

        supply  <- data$continuous_token_supply [i]
        balance <- data$connector_token_balance [i]

        ## amount of video tokens to destroy
        continuous_token_return_amount <- supply * returns_parameter

        returns <- calculate_sale_return (supply,
                                          balance,
                                          cw,
                                          continuous_token_return_amount)

        ## cat("Burning ", continuous_token_return_amount, " video tokens ", "Returning ", returns$return, " tokens \n")

        ## balance shouldn't go below 0 even if returning entire supply
        data$connector_token_balance [i] <- balance - returns$return
        data$continuous_token_supply [i] <- supply - continuous_token_return_amount

        ## earnings denominated in tokens
        earnings$creator <- earnings$creator + creator_commission * returns$return
        earnings$operator   <- earnings$operator + operator_commission * returns$return

        first_investor_return_amount <- earnings$first_investor$holdings * returns_parameter
        earnings$first_investor$earnings <- earnings$first_investor$earnings + calculate_sale_return (supply,
                                                                                                      balance,
                                                                                                      cw,
                                                                                                      first_investor_return_amount)$return
        earnings$first_investor$holdings <- earnings$first_investor$holdings - first_investor_return_amount

        ## cat("Balance ", data$connector_token_balance [i], "Supply:", data$continuous_token_supply [i], "\n")

    }

}


data <- as.data.frame(data)

head(data)

plot(
    x = data$continuous_token_supply,
    y = data$continuous_token_price,
    pch = 19,
    xlab = "Supply",
    ylab = "Price"
)

earning

init_deposit
