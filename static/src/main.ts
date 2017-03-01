import {run} from '@cycle/run';
import xs from 'xstream';
import {div, button, h1, h4, ul, li, p, a, form, input, makeDOMDriver, DOMSource} from '@cycle/dom';
import {makeHTTPDriver, Response, HTTPSource} from '@cycle/http';

type UserData = {
    userId: number,
    userName: string
};

function renderUser(user :UserData)  {
    return div('.user-details', [
        h1('.user-name', user.userName),
        h4('.user-id', user.userId),
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
        .map(res => res.body as UserData[])
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
