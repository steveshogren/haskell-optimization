var cards = $(".card");
var realCards = [];
var cardStrs = [];
cards.each(function(i, elm) {
    var card = $(this);
    var name = card.find(".t-n").text();
    var cost = card.attr("price");
    var afinity = card.attr("school");
    var does = card.find("tr:nth-child(3)").text();
    var does2 = card.find("tr:nth-child(4)").text();
    realCards.push({name: name, cost: cost, afinity: afinity, does: does});
    //$("#contents").append(name + ', ' +  cost + ', ' +  afinity + ', ' +   does + '|' +  does2 + '<br>');
    cardStrs.push(name + ', ' +  cost + ', ' +  afinity + ', ' +   does + '|' +  does2 + '<br>');
});

//console.log(realCards[0]);
console.log(cardStrs[0]);
