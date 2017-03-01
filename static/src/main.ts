import {run} from '@cycle/run';
import xs from 'xstream';
import {div, table, tr, td, th, button, h1, h4, ul, li, p, a, form, input, makeDOMDriver, DOMSource} from '@cycle/dom';
import {makeHTTPDriver, Response, HTTPSource} from '@cycle/http';

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
    ]);
}

function renderInput() {
}

function main(sources: {DOM: DOMSource, HTTP: HTTPSource}) {
    const dpsButton$ = sources.DOM.select('.get-dps').events('click');

    const ward$ = sources.DOM.select('.ward')
        .events('change')
        .map((ev:Event) => {
            return (ev.target as any).checked;
        }).startWith(false);

    const blink$ = sources.DOM.select('.blink')
        .events('change')
        .map((ev:Event) => {
            return (ev.target as any).checked;
        }).startWith(false);

    const getDps$ = xs.combine(dpsButton$, ward$, blink$)
        .map(([clickedEvent, ward, blink]) => {
            return {
                url: '/dps',
                category: 'dps',
                method: 'POST',
                send: {blink: blink, ward: ward}
            };
        });

    const requestGetdps$ = sources.HTTP.select('dps')
        .flatten()
        .map(res => res.body as Build[])
        .startWith([]);

    const state$ = xs.combine(requestGetdps$, ward$, blink$)
        .map(([dps, ward, blink]) => {
            return {dps: dps, ward: ward, blink:blink};
        });

    const header = th('.header', [td('power'), td('speed'), td('crit'), td('pen'), td('lifesteal'), td('crit_bonus'), td('ward'), td('blink')]);

    const vdom$ = state$
        .map(({dps, ward, blink}) =>

             div('.dps', [
                 div([
                     input('.ward', {attrs: {type: 'checkbox'}}), 'Ward?',
                     input('.blink', {attrs: {type: 'checkbox'}}), 'Blink?',
                 ]),
                 button('.get-dps', 'Get dps'),

                 div('.build-details', [
                     table('.builds',
                         dps==null?null:dps.map((item, idx) => renderDps(item))
                     )
                 ])
             ]));

    return {
        DOM: vdom$,
        HTTP: getDps$,
    };
}

run(main, {
    DOM: makeDOMDriver('#main-container'),
    HTTP: makeHTTPDriver(),
});
