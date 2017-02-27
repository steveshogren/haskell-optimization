import {run} from '@cycle/run';
import {Stream} from 'xstream';
import {div, button, h1, h4, ul, li, a, makeDOMDriver, DOMSource} from '@cycle/dom';
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

function main(sources: {DOM: DOMSource, HTTP: HTTPSource}) {
  const getUsers$ = sources.DOM.select('.get-users').events('click')
    .map(() => {
      return {
        url: '/users',
        category: 'users',
        method: 'GET',
      };
    });

  const user$ = sources.HTTP.select('users')
    .flatten()
    .map(res => res.body as UserData[])
    .startWith([]);

  const vdom$ = user$.map(users =>
    div('.users', [
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
