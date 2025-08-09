import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import '@testing-library/jest-dom';
import BrandingBanner from './BrandingBanner';

const renderWithRouter = (component) => {
  return render(
    <BrowserRouter>
      {component}
    </BrowserRouter>
  );
};

describe('BrandingBanner', () => {
  describe('rendering', () => {
    it('should render the application name', () => {
      renderWithRouter(<BrandingBanner />);
      expect(screen.getByText('Savage Worlds Virtual Table Top')).toBeInTheDocument();
    });

    it('should render as a clickable banner', () => {
      renderWithRouter(<BrandingBanner />);
      const banner = screen.getByRole('banner');
      expect(banner).toBeInTheDocument();
    });

    it('should have consistent branding styling', () => {
      renderWithRouter(<BrandingBanner />);
      const banner = screen.getByTestId('branding-banner');
      expect(banner).toHaveClass('branding-banner');
    });
  });

  describe('navigation', () => {
    it('should link to marketing page by default', () => {
      renderWithRouter(<BrandingBanner />);
      const bannerLink = screen.getByRole('link');
      expect(bannerLink).toHaveAttribute('href', '/');
    });

    it('should accept custom navigation path', () => {
      renderWithRouter(<BrandingBanner to="/custom" />);
      const bannerLink = screen.getByRole('link');
      expect(bannerLink).toHaveAttribute('href', '/custom');
    });

    it('should navigate when clicked', () => {
      const mockNavigate = jest.fn();
      jest.doMock('react-router-dom', () => ({
        ...jest.requireActual('react-router-dom'),
        useNavigate: () => mockNavigate
      }));

      renderWithRouter(<BrandingBanner />);
      const banner = screen.getByRole('link');
      fireEvent.click(banner);
      
      expect(banner).toHaveAttribute('href', '/');
    });
  });

  describe('logo display', () => {
    it('should display logo when showLogo is true', () => {
      renderWithRouter(<BrandingBanner showLogo={true} />);
      const logo = screen.getByRole('img', { name: /savage worlds logo/i });
      expect(logo).toBeInTheDocument();
    });

    it('should not display logo when showLogo is false', () => {
      renderWithRouter(<BrandingBanner showLogo={false} />);
      const logo = screen.queryByRole('img', { name: /savage worlds logo/i });
      expect(logo).not.toBeInTheDocument();
    });

    it('should display logo by default', () => {
      renderWithRouter(<BrandingBanner />);
      const logo = screen.getByRole('img', { name: /savage worlds logo/i });
      expect(logo).toBeInTheDocument();
    });
  });

  describe('styling variants', () => {
    it('should apply compact styling when compact prop is true', () => {
      renderWithRouter(<BrandingBanner compact={true} />);
      const banner = screen.getByTestId('branding-banner');
      expect(banner).toHaveClass('compact');
    });

    it('should not have compact styling by default', () => {
      renderWithRouter(<BrandingBanner />);
      const banner = screen.getByTestId('branding-banner');
      expect(banner).not.toHaveClass('compact');
    });

    it('should accept custom className', () => {
      renderWithRouter(<BrandingBanner className="custom-banner" />);
      const banner = screen.getByTestId('branding-banner');
      expect(banner).toHaveClass('custom-banner');
      expect(banner).toHaveClass('branding-banner');
    });
  });

  describe('accessibility', () => {
    it('should have proper banner role', () => {
      renderWithRouter(<BrandingBanner />);
      expect(screen.getByRole('banner')).toBeInTheDocument();
    });

    it('should have accessible link text', () => {
      renderWithRouter(<BrandingBanner />);
      const link = screen.getByRole('link');
      expect(link).toHaveAccessibleName();
    });

    it('should have alt text for logo', () => {
      renderWithRouter(<BrandingBanner showLogo={true} />);
      const logo = screen.getByRole('img');
      expect(logo).toHaveAttribute('alt');
      expect(logo.getAttribute('alt')).toBeTruthy();
    });
  });

  describe('responsive behavior', () => {
    it('should have responsive classes', () => {
      renderWithRouter(<BrandingBanner />);
      const banner = screen.getByTestId('branding-banner');
      expect(banner).toHaveClass('d-flex');
    });
  });
});