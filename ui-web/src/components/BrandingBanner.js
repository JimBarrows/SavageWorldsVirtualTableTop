import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Link } from 'react-router-dom';
import './BrandingBanner.css';

class BrandingBanner extends Component {
  static propTypes = {
    to: PropTypes.string,
    showLogo: PropTypes.bool,
    compact: PropTypes.bool,
    className: PropTypes.string
  };

  static defaultProps = {
    to: '/',
    showLogo: true,
    compact: false,
    className: ''
  };

  render() {
    const { to, showLogo, compact, className } = this.props;
    
    const bannerClasses = [
      'branding-banner',
      'd-flex',
      'align-items-center',
      'justify-content-center',
      'text-decoration-none',
      compact ? 'compact' : '',
      className
    ].filter(Boolean).join(' ');

    return (
      <header role="banner">
        <Link 
          to={to}
          className={bannerClasses}
          data-testid="branding-banner"
          aria-label="Savage Worlds Virtual Table Top - Go to home page"
        >
          {showLogo && (
            <img 
              src="/assets/images/savage-worlds-logo-small.png"
              alt="Savage Worlds Logo"
              className="banner-logo me-3"
              role="img"
            />
          )}
          <div className="banner-text">
            <h1 className="banner-title mb-0">Savage Worlds Virtual Table Top</h1>
            {!compact && (
              <p className="banner-subtitle mb-0 text-muted">
                Your Digital RPG Companion
              </p>
            )}
          </div>
        </Link>
      </header>
    );
  }
}

export default BrandingBanner;