# Marketing Assets

This directory contains the marketing and branding assets for the Savage Worlds Virtual Table Top application.

## Required Images

### Logos
- `savage-worlds-logo.png` - Main application logo (200px wide, transparent background)
- `savage-worlds-logo-small.png` - Compact banner logo (50px height, transparent background)

### Marketing Materials
- `swvtt-screenshot.png` - Application screenshot for marketing page (800x600px recommended)

## Image Guidelines

### Logo Requirements
- **Format**: PNG with transparent background
- **Colors**: Should maintain brand consistency
- **Usage**: Logo files should be optimized for web use
- **Copyright**: Ensure proper licensing for Savage Worlds branding elements

### Screenshot Requirements
- **Format**: PNG or JPEG
- **Size**: Optimized for web (under 500KB)
- **Content**: Show key application features like plot point management
- **Quality**: High resolution but web-optimized

## Brand Colors

The application uses the following color palette:
- **Primary Gradient**: #667eea to #764ba2 (hero sections)
- **Accent Color**: #e74c3c (buttons and highlights)
- **Text Colors**: #2c3e50 (headers), #6c757d (body text)
- **Background**: #f8f9fa (light sections), white (main content)

## Implementation Notes

1. All images should be placed directly in this directory
2. File names must match exactly as referenced in the components:
   - MarketingPage.js references these image paths
   - BrandingBanner.js uses the small logo variant
3. Ensure images are optimized for web to maintain fast load times
4. Test images across different screen sizes and devices

## Fallback Behavior

If images are not available, the components will:
- Show alt text for accessibility
- Maintain layout structure without breaking
- Display gracefully with text-only content

Update this README when adding new marketing assets or changing requirements.