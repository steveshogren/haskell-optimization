var cards = $(".card");
var realCards = [];
cards.each(function(i, elm) {
    var card = $(this);
    var name = card.find(".t-n").text();
    var cost = card.find(".t-p").text();
    var afinity = card.find(".t-c").text();
    var does = card.find("tr:nth-child(3)").text();
    var does2 = card.find("tr:nth-child(4)").text();
    realCards.push({name: name, cost: cost, afinity: afinity, does: does});
    $("#contents").append(name + ', ' +  cost + ', ' +  afinity + ', ' +   does + '|' +  does2 + '<br>');
});

