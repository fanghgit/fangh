#********poker*******
set.seed(10)
allcards = sample(rep(c(1:13),4))
player1 = allcards[1:(length(allcards)/2)]
player2 = allcards[-(1:(length(allcards)/2))]

k = 0
i = 1
cards = player1[1]
player1 = player1[-1]
while(length(player1) != 0 && length(player2) != 0){
  i = i + 1
  k = k + 1
  if(i %% 2 == 0 && !is.null(cards) && player2[1] %in% cards)
  {
    flag = player2[1]
    cards = append(cards, player2[1])
    tmp = cards[which(cards == player2[1])[1]:which(cards == player2[1])[2]]
    player2 = player2[-1]
    player2 = append(player2, tmp)
    i = 1
    cards = cards[-(which(cards == flag)[1]:which(cards == flag)[2])]
  }
  else if(i %% 2 == 0)
  {
    cards = append(cards, player2[1])
    player2 = player2[-1]
  }
  else if(i %% 2 == 1 && !is.null(cards) && player1[1] %in% cards)
  {
    flag = player1[1]
    cards = append(cards, player1[1])
    tmp = cards[which(cards == player1[1])[1]:which(cards == player1[1])[2]]
    player1 = player1[-1]
    player1 = append(player1, tmp)
    i = 0
    cards = cards[-(which(cards == flag)[1]:which(cards == flag)[2])]
  }
  else
  {
    cards = append(cards, player1[1])
    player1 = player1[-1]
  }
  print("k")
  print(k)
  print("player1")
  print(player1)
  print("player2")
  print(player2)
  print("cards")
  print(cards)
}



sum = 0
for(i in 3:7)
{
  sum = choose(10, i)*0.5^10 + sum
}



