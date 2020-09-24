import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

let app
fetch('/api/init_state').then(res=>res.json()).then(res=>{
  app = Elm.Main.init({
    node: document.getElementById('root'),
    flags: res.users,
  });
})



// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

