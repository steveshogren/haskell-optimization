import {run} from '@cycle/run';
import xs from 'xstream';
import {div, table, tr, td, th, button, h, h1, h4, ul, li, p, a, form, input, makeDOMDriver, DOMSource} from '@cycle/dom';
import {makeHTTPDriver, Response, HTTPSource} from '@cycle/http';

type State = {
    ward: boolean,
    blink: boolean,
    dps: number
};

type HandCard = {
  count : number,
  info : string
 }

type Build = {
    _bdps: number,
    _bpower: number,
    _bspeed: number,
    _bcrit: number,
    _bpen: number,
    _blifesteal: number,
    _bcrit_bonus: number,
    _bward: number,
    _bblink: number
};

function renderDps(build :Build)  {
    return tr('.build', [
        td(build._bdps),
        td('.power', build._bpower),
        td('.speed', build._bspeed),
        td('.crit', build._bcrit),
        td('.pen', build._bpen),
        td('.lifesteal', build._blifesteal),
        td('.crit_bonus', build._bcrit_bonus),
        td('.ward', build._bward),
        td('.blink', build._bblink),
        button('.optimize', {attrs: {'data-build': JSON.stringify(build)}}, 'Optimize')
    ]);
}

function checkboxBoolean(checkbox$ : DOMSource) {
    return checkbox$
        .events('change')
        .map((ev:Event) => {
            return (ev.target as any).checked;
        }).startWith(false);
}

function view(state$ : any) {
    return state$
        .map(({hand, dps, ward, blink, optBuild}) => {
            var header = [tr('.header', [th('dps'), th('power'), th('speed'), th('crit'), th('pen'), th('lifesteal'), th('crit_bonus'), th('ward'), th('blink')])];
            var tdata = dps==null?[]:dps.map((item, idx) => renderDps(item))
            var hand = hand.length==0?[li("No results")]:hand.map((item, idx) => li([item.count, item.info]))
            return div('.dps', [
                div([
                    input('.ward', {attrs: {type: 'checkbox'}}), 'Ward?',
                    input('.blink', {attrs: {type: 'checkbox'}}), 'Blink?',
                ]),
                button('.get-dps', 'Get dps'),

                div('.build-details', [
                    table('.builds', header.concat(tdata))
                ]),

                ul(hand)
            ])
        });
}

function intent(dom) {
    const optimizeClicked$ = dom.select('.optimize')
        .events('click')
        .map(event => {
            return {
                url: '/optimize',
                category: 'optimize',
                method: 'POST',
                send: JSON.parse(event.target.dataset.build)
            };
        });

    const changeWard$ = checkboxBoolean(dom.select('.ward'));
    const changeBlink$ = checkboxBoolean(dom.select('.blink'));

    const getDps$ = xs.combine(dom.select('.get-dps').events('click'), changeWard$, changeBlink$)
        .map(([clickedEvent, ward, blink]) => {
            return {
                url: '/dps',
                category: 'dps',
                method: 'POST',
                send: {blink: blink, ward: ward}
            };
        });

    return {
        optimizeClicked$ : optimizeClicked$,
        changeWard$ : changeWard$,
        changeBlink$ : changeBlink$,
        httpRequest$ : xs.merge(optimizeClicked$, getDps$)
    }
}

function model(http, actions) {
    const requestGetdps$ = http.select('dps')
        .flatten()
        .map(res => res.body as Build[])
        .startWith([]);
    const requestOptimize$ = http.select('optimize')
        .flatten()
        .map(res => {
            return res.body as HandCard[]
        })
        .startWith([]);
    return xs.combine(requestOptimize$, requestGetdps$, actions.changeWard$, actions.changeBlink$)
        .map(([hand, dps, ward, blink]) => {
            return {hand: hand, dps: dps, ward: ward, blink:blink};
        });
}

function main(sources: {DOM: DOMSource, HTTP: HTTPSource}) {

    const actions = intent(sources.DOM);
    const state$ = model(sources.HTTP, actions);
    const vdom$ = view(state$);

    return {
        DOM: vdom$,
        HTTP: actions.httpRequest$,
    };
}

run(main, {
    DOM: makeDOMDriver('#main-container'),
    HTTP: makeHTTPDriver(),
});
