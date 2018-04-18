import {ApolloClient} from 'apollo-client';
import {HttpLink, InMemoryCache} from 'apollo-client-preset';
import 'bootstrap/dist/css/bootstrap-theme.min.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import React from 'react';
import {ApolloProvider} from 'react-apollo';
import ReactDOM from 'react-dom';
import {BrowserRouter} from 'react-router-dom';
import App from './App';
import './index.css';
import registerServiceWorker from './registerServiceWorker';

const httpLink = new HttpLink({uri: 'http://localhost:4000'});

const client = new ApolloClient({
                                  link: httpLink,
                                  cache: new InMemoryCache()
});

ReactDOM.render(<BrowserRouter >
  <ApolloProvider client={client} >
    <App />
  </ApolloProvider >
</BrowserRouter>, document.getElementById('root'));
registerServiceWorker();
