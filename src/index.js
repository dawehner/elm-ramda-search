import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import ramda from './ramda.json';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: ramda,
});

registerServiceWorker();
