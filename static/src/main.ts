import {run} from '@cycle/run';
import xs from 'xstream';
import {div, button, h1, h4, ul, li, p, a, form, input, makeDOMDriver, DOMSource} from '@cycle/dom';
import {makeHTTPDriver, Response, HTTPSource} from '@cycle/http';

type Build = {
    _bpower: number,
    _bspeed: number,
    _bcrit: number,
    _bpen: number,
    _blifesteal: number,
    _bcrit_bonus: number,
    _bward: number,
    _bblink: number
};

function renderUser(user :Build)  {
    return div('.user-details', [
        ul('.build', [
            li('.power', user._bpower),
            li('.speed', user._bspeed),
            li('.crit', user._bcrit),
            li('.pen', user._bpen),
            li('.lifesteal', user._blifesteal),
            li('.crit_bonus', user._bcrit_bonus),
            li('.ward', user._bward),
            li('.blink', user._bblink),
        ])
    ]);
}

function renderInput() {
}

function main(sources: {DOM: DOMSource, HTTP: HTTPSource}) {
    const getUsers$ = sources.DOM.select('.get-users').events('click')
        .map(() => {
            return {
                url: '/users',
                category: 'users',
                method: 'GET',
            };
        });

    const checkers$ = sources.DOM.select('.checker')
        .events('change')
        .map((ev:Event) => {
            return (ev.target as any).checked;
        }).startWith(false);

    const user$ = sources.HTTP.select('users')
        .flatten()
        .map(res => [res.body] as Build[])
        .startWith([]);

    const state$ = xs.combine(user$, checkers$)
        .map(([users, toggled]) => {
            return {users: users, toggled: toggled};
        });

    const vdom$ = state$
        .map(({users, toggled}) =>
             div('.users', [
                 div([
                     input('.checker', {attrs: {type: 'checkbox'}}), 'Toggle Me',
                     p(toggled ? 'ON': 'OFF'),
                 ]),
                 button('.get-users', 'Get users'),
                 ul("userslist", users.map((item, idx) => renderUser(item))),
             ]),
            );

    return {
        DOM: vdom$,
        HTTP: getUsers$,
    };
}

run(main, {
    DOM: makeDOMDriver('#main-container'),
    HTTP: makeHTTPDriver(),
});
