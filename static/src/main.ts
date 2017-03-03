import {run} from '@cycle/run';
import xs from 'xstream';
import {div, table, tr, td, th, button, h, h1, h4, ul, li, p, a, form, input, makeDOMDriver, DOMSource} from '@cycle/dom';
import {makeHTTPDriver, Response, HTTPSource} from '@cycle/http';

type State = {
    ward: boolean,
    blink: boolean,
    dps: number
};

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
        h('button.optimize', {props: {style: 'test'}}, 'Optimize')
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
        .map(({dps, ward, blink}) => {
            var header = [tr('.header', [th('power'), th('speed'), th('crit'), th('pen'), th('lifesteal'), th('crit_bonus'), th('ward'), th('blink')])];
            var tdata = dps==null?[]:dps.map((item, idx) => renderDps(item))
            return div('.dps', [
                div([
                    button('.doer', 'OER'),
                    input('.ward', {attrs: {type: 'checkbox'}}), 'Ward?',
                    input('.blink', {attrs: {type: 'checkbox'}}), 'Blink?',
                ]),
                button('.get-dps', 'Get dps'),

                div('.build-details', [
                    table('.builds', header.concat(tdata))
                ])
            ])
        });
}

function intent(dom) {
    const getDpsClicked$ = dom.select('.get-dps').events('click');
    const optimizeClicked$ = dom.select('.optimize')
        .events('click')
        .map(event => {

            console.log("test");
            debugger;
            return false;
        }).startWith(false);

    const changeWard$  =  checkboxBoolean(dom.select('.ward'));
    const changeBlink$  =  checkboxBoolean(dom.select('.blink'));

    const getDps$ = xs.combine(optimizeClicked$, getDpsClicked$, changeWard$, changeBlink$)
        .map(([_, clickedEvent, ward, blink]) => {
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
        httpRequest$ : getDps$
    }
}

function model(http, actions) {
    const requestGetdps$ = http.select('dps')
        .flatten()
        .map(res => res.body as Build[])
        .startWith([]);
    return xs.combine(requestGetdps$, actions.changeWard$, actions.changeBlink$)
        .map(([dps, ward, blink]) => {
            return {dps: dps, ward: ward, blink:blink};
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
