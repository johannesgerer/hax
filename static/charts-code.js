
Chart.defaults.global.animation = false;

var accuracy = 3;

var baseScale = 1000;
var accountScales = {
    'default'       : 1,
};
function accountScale(account) {
    return baseScale * (accountScales[account] || accountScales['default']);
}

var accountColors = {
    'Verm'          : "rgba(150, 0, 0, 1)",
    'Giro'          : "rgba(150, 0, 0, 0.5)",
    'Lohn'          : "rgba(255, 0, 0, 1)",
    'LSt'           : "rgba(255, 0, 0, 0.5)",
    'StFrei'        : "rgba(255, 0, 0, 0.2)",
    'default'       : "rgba(150, 150, 150, 0.5)",
};
function accountColor(account) {
    return accountColors[account] || accountColors['default'];
}

function arrayMult(arr, m) {
    var a = arr.slice(0);
    for(var i=0; i<a.length; i++) {
        a[i] *= m;
    }
    return a;
}
function arrayAbsSum(arr) {
    return arr.reduce(function(prev, current) {
        return Math.abs(current) + prev;
    }, 0);
}

var months = [], accountBalances = {}, plotData = {};
$(document).ready(function(j) {

    var ctx = $("#myChart")[0].getContext("2d");
    var myNewChart;
    for(var a in data.accounts) {
        var account = data.accounts[a].fAccount;
        if("Month" != account) {
            accountBalances[account] = [];
        }
    }
    var i = 0;
    for(var b in data.balances) {
        if(i++ % accuracy != 0) continue;
        var balance = data.balances[b];
        for(var a in data.accounts) {
            var account = data.accounts[a].fAccount;
            if("Month" == account) months.push(balance[a]);
            else accountBalances[account].push(balance[a]);
        }
    }
    for(var a in data.accounts) {
        var account = data.accounts[a].fAccount;
        if("Month" != account) {
            $("<label>").css({'color': accountColor(account)})
                .text(" "+account + " ("+Math.round(arrayAbsSum(accountBalances[account]))+")")
                .prepend(
                    $("<input>", { type: "checkbox", name: account})
                        .prop('checked', false)
                        .prop('data-account', account)
                        .click(function() {
                            redrawChart();
                        })
                ).appendTo("#accountSelectorForm");
        }
    }
    $("#accountSelectorForm #checkAll").click(function() {
        $("#accountSelectorForm input:checkbox:not(#checkAll)")
            .prop('checked', $(this).is(":checked"));
        redrawChart();
    });

    function redrawChart() {
        if(myNewChart) myNewChart.destroy();
        plotData.labels = months;
        plotData.datasets = [];
        $("#accountSelectorForm input:checkbox:not(#checkAll)").each(function() {
            if(!$(this).is(":checked")) return true;
            var account = $(this).prop('data-account');
            plotData.datasets.push(
                {
                    label: account,
                    data: arrayMult(accountBalances[account], accountScale(account)),
                    strokeColor: accountColor(account),
                    pointColor: accountColor(account),
                    pointStrokeColor: "#fff",
                    pointHighlightFill: "#fff",
                    pointHighlightStroke: accountColor(account),
                }
            );
        });
        if(plotData.datasets.length>0)
            myNewChart = new Chart(ctx).Line(plotData, {
                datasetFill: false
            });
    }


});
