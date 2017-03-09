import {run} from '@cycle/run';
import xs from 'xstream';
import {div, table, tr, td, th, select, option,button, h, h1, h4, ul, li, p, a, form, input, makeDOMDriver, DOMSource} from '@cycle/dom';
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

function lifeStealDropDown() {
    return input('.ls', {attrs: {type: 'number', min:0, max:20}}, 0)
}

function view(state$ : any) {
    return state$
        .map(({hand, dps, ward, blink, optBuild, lifesteal, loading, hero}) => {
            var header = [tr('.header', [th('dps'), th('power'), th('speed'), th('crit'), th('pen'), th('lifesteal'), th('crit_bonus'), th('ward'), th('blink')])];
            var tdata = dps==null?[]:dps.map((item, idx) => renderDps(item))
            var hand = ul(hand.length==0?[]:hand.map((item, idx) => li([item.count, item.info])))

            return div('.dps', [
                div([
                    div([select('.hero', [option({attrs: {value: 'murdock'}}, 'Murdock'),
                                          option({attrs: {value: 'sparrow'}}, 'Sparrow'),
                                          option({attrs: {value: 'twinblast'}}, 'Twinblast'),
                                          option({attrs: {value: 'grim'}}, 'Grim.EXE'),
                                         ])
                         , 'Hero?']),
                    div([input('.ward', {attrs: {type: 'checkbox'}}), 'Ward?']),
                    div([input('.blink', {attrs: {type: 'checkbox'}}), 'Blink?']),
                    div([lifeStealDropDown(), 'Lifesteal'])
                ]),
                button('.get-dps', 'Get dps'),

                div('.build-details', [
                    table('.builds', header.concat(tdata))
                ]),

                (loading? "Loading" : hand)

            ])
        });
}

function intent(dom) {
    const optimizeRequest$ = dom.select('.optimize')
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
    const changeLs$ = dom.select('.ls')
        .events('change')
        .map((ev:Event) => {
            var i = parseInt((ev.target as any).value);
            return i || 0;
        }).startWith(0);

    const changeHero$ = dom.select('.hero')
        .events('change')
        .map((ev:Event) => {
            var i = (ev.target as any).value;
            return i || 'murdock';
        }).startWith('murdock');


    const dpsRequest$ = xs.combine(dom.select('.get-dps').events('click'),
                                   changeWard$,
                                   changeBlink$,
                                   changeLs$,
                                   changeHero$)
        .map(([clickedEvent, ward, blink, ls, hero]) => {
            return {
                url: '/dps',
                category: 'dps',
                method: 'POST',
                send: {has_blink: blink, has_ward: ward, desired_lifesteal: ls, hero_name: hero}
            };
        });

    return {
        optimizeRequest$ : optimizeRequest$,
        dpsRequest$ : dpsRequest$,
        changeWard$ : changeWard$,
        changeHero$ : changeHero$,
        changeBlink$ : changeBlink$,
        changeLs$ : changeLs$,
        httpRequest$ : xs.merge(optimizeRequest$, dpsRequest$)
    }
}

function model(http, actions) {
    const dpsResponse$ = http.select('dps')
        .flatten()
        .map(res => res.body as Build[])
        .startWith([]);
    const optimizeResponse$ = http.select('optimize')
        .flatten()
        .map(res => {
            return res.body as HandCard[]
        })
        .startWith([]);

    const loadingOptimize$ = xs.merge(actions.optimizeRequest$.mapTo(true),
                                      optimizeResponse$.mapTo(false),
                                      actions.dpsRequest$.mapTo(true),
                                      dpsResponse$.mapTo(false))
        .startWith(false);

    const dpsValues$ = xs.merge(dpsResponse$, actions.dpsRequest$.mapTo([]));
    const optimizeValues$ = xs.merge(optimizeResponse$, dpsResponse$.mapTo([]));

    return xs.combine(optimizeValues$, dpsValues$,
                      actions.changeWard$, actions.changeBlink$,
                      actions.changeLs$, loadingOptimize$,
                      actions.changeHero$)
        .map(([hand, dps, ward, blink, ls, loading, hero]) => {
            return {hand: hand, dps: dps, ward: ward, blink:blink, lifesteal: ls, loading:loading, hero:hero};
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
