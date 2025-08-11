import React from 'react';
import { render, screen } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import '@testing-library/jest-dom';
import MarketingPage from './MarketingPage';

const renderWithRouter = (component) => {
  return render(
    <BrowserRouter>
      {component}
    </BrowserRouter>
  );
};

describe('MarketingPage', () => {
  describe('rendering', () => {
    it('should render the application title', () => {
      renderWithRouter(<MarketingPage />);
      expect(screen.getByText('Savage Worlds Virtual Table Top')).toBeInTheDocument();
    });

    it('should render marketing headline', () => {
      renderWithRouter(<MarketingPage />);
      expect(screen.getByRole('heading', { level: 1 })).toBeInTheDocument();
    });

    it('should render Sign Up Now call-to-action button', () => {
      renderWithRouter(<MarketingPage />);
      expect(screen.getByRole('link', { name: /sign up now/i })).toBeInTheDocument();
    });

    it('should render Login link', () => {
      renderWithRouter(<MarketingPage />);
      expect(screen.getByRole('link', { name: /login/i })).toBeInTheDocument();
    });
  });

  describe('features section', () => {
    it('should display key application features', () => {
      renderWithRouter(<MarketingPage />);
      
      expect(screen.getByText(/plot point management/i)).toBeInTheDocument();
      expect(screen.getByText(/character creation/i)).toBeInTheDocument();
      expect(screen.getByText(/beast management/i)).toBeInTheDocument();
      expect(screen.getByText(/equipment tracking/i)).toBeInTheDocument();
    });

    it('should display feature descriptions', () => {
      renderWithRouter(<MarketingPage />);
      
      const features = screen.getAllByRole('listitem');
      expect(features.length).toBeGreaterThanOrEqual(4);
    });
  });

  describe('branding elements', () => {
    it('should display application logo', () => {
      renderWithRouter(<MarketingPage />);
      const logo = screen.getByRole('img', { name: /logo/i });
      expect(logo).toBeInTheDocument();
    });

    it('should have consistent branding colors', () => {
      renderWithRouter(<MarketingPage />);
      const page = screen.getByTestId('marketing-page');
      expect(page).toHaveClass('marketing-page');
    });
  });

  describe('navigation', () => {
    it('should have correct href for signup button', () => {
      renderWithRouter(<MarketingPage />);
      const signupLink = screen.getByRole('link', { name: /sign up now/i });
      expect(signupLink).toHaveAttribute('href', '/signup');
    });

    it('should have correct href for login link', () => {
      renderWithRouter(<MarketingPage />);
      const loginLink = screen.getByRole('link', { name: /login/i });
      expect(loginLink).toHaveAttribute('href', '/login');
    });
  });

  describe('accessibility', () => {
    it('should have proper semantic structure', () => {
      renderWithRouter(<MarketingPage />);
      
      expect(screen.getByRole('main')).toBeInTheDocument();
      expect(screen.getByRole('heading', { level: 1 })).toBeInTheDocument();
    });

    it('should have alt text for images', () => {
      renderWithRouter(<MarketingPage />);
      const images = screen.getAllByRole('img');
      images.forEach(img => {
        expect(img).toHaveAttribute('alt');
        expect(img.getAttribute('alt')).toBeTruthy();
      });
    });
  });

  describe('responsive design', () => {
    it('should have responsive container classes', () => {
      renderWithRouter(<MarketingPage />);
      const container = screen.getByTestId('marketing-page');
      expect(container).toHaveClass('container-fluid');
    });
  });
});