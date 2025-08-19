import React from 'react';
import { render } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import App from './src/App';

const { container } = render(
  <MemoryRouter>
    <App />
  </MemoryRouter>
);

// Log the HTML to see what's actually rendered
console.log("App structure classes:", Array.from(container.querySelectorAll('[class]')).map(el => el.className).slice(0, 10));
console.log("Has .App?", !!container.querySelector('.App'));
console.log("First child tag:", container.firstChild?.tagName);
console.log("First child class:", container.firstChild?.className);
