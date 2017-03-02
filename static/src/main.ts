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

function checkboxBoolean(checkbox$ : DOMSource) {
    return checkbox$
        .events('change', )
        .map((ev:Event) => {
            return (ev.target as any).checked;
        }).startWith(false);
}

function main(sources: {DOM: DOMSource, HTTP: HTTPSource}) {
    const dpsButton$ = sources.DOM.select('.get-dps').events('click');

    const ward$ = checkboxBoolean(sources.DOM.select('.ward'));
    const blink$ = checkboxBoolean(sources.DOM.select('.blink'));

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


    const vdom$ = state$
        .map(({dps, ward, blink}) => {

            var header = [tr('.header', [th('power'), th('speed'), th('crit'), th('pen'), th('lifesteal'), th('crit_bonus'), th('ward'), th('blink')])];
            var tdata = dps==null?[]:dps.map((item, idx) => renderDps(item))
            return div('.dps', [
                div([
                    input('.ward', {attrs: {type: 'checkbox'}}), 'Ward?',
                    input('.blink', {attrs: {type: 'checkbox'}}), 'Blink?',
                ]),
                button('.get-dps', 'Get dps'),

                div('.build-details', [
                    table('.builds', header.concat(tdata))
                ])
            ])});

    return {
        DOM: vdom$,
        HTTP: getDps$,
    };
}

run(main, {
    DOM: makeDOMDriver('#main-container'),
    HTTP: makeHTTPDriver(),
});
