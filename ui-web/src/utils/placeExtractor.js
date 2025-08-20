/**
 * Extracts potential place names from story text for Game Master reference
 * Following DDD principles: this utility operates within the Plot Point bounded context
 * Using ubiquitous language: Story, Places, Game Master
 */

/**
 * Common words that should not be considered place names
 * These represent domain knowledge about what constitutes a place vs. other story elements
 */
const EXCLUDED_WORDS = new Set([
  // Time references
  'January', 'February', 'March', 'April', 'May', 'June',
  'July', 'August', 'September', 'October', 'November', 'December',
  'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday',
  'Winter', 'Spring', 'Summer', 'Fall', 'Autumn',
  
  // Common fantasy terms that are not places (when standalone)
  'Quest', 'Adventure', 'Journey', 'Artifact', 'Shield', 'Magic',
  'Ancient', 'Mysterious', 'Stranger', 'Hero', 'Heroes', 'Dragon', 'Treasure',
  'Spell', 'Potion', 'Weapon', 'Armor', 'Crown', 'Ring', 'Amulet',
  
  // Common story elements
  'Beginning', 'End', 'Chapter', 'Story', 'Tale', 'Legend', 'Myth',
  'Battle', 'War', 'Peace', 'Victory', 'Defeat', 'Challenge',
  
  // Character roles and titles
  'King', 'Queen', 'Prince', 'Princess', 'Lord', 'Lady', 'Sir', 'Dame',
  'Knight', 'Wizard', 'Priest', 'Warrior', 'Rogue', 'Paladin',
  
  // Generic descriptors and common words
  'Great', 'Grand', 'Mighty', 'Powerful', 'Sacred', 'Holy', 'Dark', 'Light',
  'The', 'They', 'Along', 'After', 'Before', 'During', 'Upon', 'Later',
  'Eventually', 'Finally', 'Then', 'When', 'Where', 'While', 'Since', 'In',
  
  // Geographic terms that are often part of place names but not places by themselves
  // Note: These should be excluded only when they appear alone, not as part of compound names
]);

/**
 * Geographic terms that should only be excluded when they appear alone
 * But are valid when part of compound place names
 */
const STANDALONE_GEOGRAPHIC_TERMS = new Set([
  'Coast', 'River', 'Mountain', 'Valley', 'Forest', 'Desert', 'Ocean', 'Sea',
  'Lake', 'Hill', 'Island', 'Bay', 'Canyon', 'Plain', 'Marsh', 'Swamp'
]);

/**
 * Fantasy terms that can form valid place names when combined with geographic terms
 */
const FANTASY_PLACE_TERMS = new Set([
  'Sword', 'Dragon', 'Moon', 'Sun', 'Star', 'Shadow', 'Light', 'Fire', 'Ice',
  'Storm', 'Thunder', 'Lightning', 'Wind', 'Stone', 'Crystal', 'Gold', 'Silver'
]);

/**
 * Extracts place names from story text using business rules for Game Masters
 * 
 * Business Rules (Domain Logic):
 * 1. Places are typically capitalized words or phrases
 * 2. Multi-word places are connected by "of", "the", articles, or hyphens
 * 3. Each place should appear only once (no duplicates)
 * 4. Case-insensitive duplicate detection
 * 5. Alphabetical sorting for Game Master convenience
 * 
 * @param {string} storyText - The story content to analyze
 * @returns {string[]} - Array of unique place names, alphabetically sorted
 */
export function extractPlacesFromStory(storyText) {
  // Handle null, undefined, or empty text
  if (!storyText || typeof storyText !== 'string' || !storyText.trim()) {
    return [];
  }

  // Step 1: Find all potential place patterns using multiple strategies
  const placeCandidates = new Set();
  
  // Remove debug variable (not needed in final version)
  
  // Strategy 1: Multi-word places with "of the" pattern (e.g., "Tower of the Moon")
  const ofThePattern = /\b[A-Z][a-zA-Z'-]*\s+of\s+the\s+[A-Z][a-zA-Z'-]*(?:\s+[A-Z][a-zA-Z'-]*)*\b/g;
  const ofTheMatches = storyText.match(ofThePattern) || [];
  ofTheMatches.forEach(match => placeCandidates.add(match.trim()));
  
  // Strategy 2: Multi-word places with "of" pattern (e.g., "River of Souls")
  const ofPattern = /\b[A-Z][a-zA-Z'-]*\s+of\s+[A-Z][a-zA-Z'-]*\b/g;
  const ofMatches = storyText.match(ofPattern) || [];
  ofMatches.forEach(match => placeCandidates.add(match.trim()));
  
  // Strategy 3: Compound place names (e.g., "Red Larch", "Sword Coast")
  const compoundPattern = /\b[A-Z][a-zA-Z'-]*\s+[A-Z][a-zA-Z'-]*\b/g;
  const compoundMatches = storyText.match(compoundPattern) || [];
  compoundMatches.forEach(match => {
    const words = match.trim().split(/\s+/);
    
    // Special handling for compound names with excluded words
    const hasGeographicTerm = words.some(word => STANDALONE_GEOGRAPHIC_TERMS.has(word));
    const hasFantasyTerm = words.some(word => FANTASY_PLACE_TERMS.has(word));
    const hasExcludedWord = words.some(word => EXCLUDED_WORDS.has(word));
    
    if (hasGeographicTerm) {
      // Geographic compound names are likely places (e.g., "Sword Coast", "Dragon Mountain")
      placeCandidates.add(match.trim());
    } else if (!hasExcludedWord) {
      // Only add if no words are excluded
      placeCandidates.add(match.trim());
    }
  });
  
  // Strategy 4: Single capitalized words (fallback)
  const singleWordPattern = /\b[A-Z][a-zA-Z'-]*\b/g;
  const singleMatches = storyText.match(singleWordPattern) || [];
  singleMatches.forEach(match => {
    const trimmed = match.trim();
    if (!EXCLUDED_WORDS.has(trimmed) && 
        trimmed.length >= 3 && 
        !placeCandidates.has(trimmed)) {
      // Only add if not already covered by a multi-word match
      const isPartOfLarger = Array.from(placeCandidates).some(place => 
        place.includes(trimmed) && place !== trimmed
      );
      if (!isPartOfLarger) {
        placeCandidates.add(trimmed);
      }
    }
  });

  // Step 2: Clean up the candidates
  const cleanedPlaces = Array.from(placeCandidates).filter(place => {
    const trimmed = place.trim();
    
    // Skip if too short
    if (trimmed.length < 3) {
      return false;
    }
    
    // Skip if starts with excluded word
    const firstWord = trimmed.split(/\s+/)[0];
    if (EXCLUDED_WORDS.has(firstWord)) {
      return false;
    }
    
    // Skip standalone geographic terms, but allow them in compound names
    const words = trimmed.split(/\s+/);
    if (words.length === 1 && STANDALONE_GEOGRAPHIC_TERMS.has(trimmed)) {
      return false;
    }
    
    // Skip possessive forms (Neverwinter's) in favor of base form
    if (trimmed.endsWith("'s")) {
      const baseName = trimmed.slice(0, -2);
      if (placeCandidates.has(baseName)) {
        return false; // Skip possessive if base form exists
      }
    }
    
    return true;
  });

  // Step 3: Remove case-insensitive duplicates, keeping first occurrence
  const uniquePlaces = [];
  const seenPlaces = new Set();
  
  for (const place of cleanedPlaces) {
    const normalized = place.toLowerCase().replace(/'s$/, ''); // Normalize possessives
    if (!seenPlaces.has(normalized)) {
      seenPlaces.add(normalized);
      // Prefer base form over possessive
      const cleanPlace = place.replace(/'s$/, '');
      uniquePlaces.push(cleanPlace);
    }
  }
  
  // Step 4: Sort alphabetically for Game Master convenience
  return uniquePlaces.sort();
}