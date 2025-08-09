export default class Modifier {
  constructor(data = {}) {
    this.name = data.name || '';
    this.description = data.description || '';
    this.powerPointModifier = data.powerPointModifier || 0;
  }

  isValid() {
    return !!(this.name && this.description);
  }

  toJSON() {
    return {
      name: this.name,
      description: this.description,
      powerPointModifier: this.powerPointModifier
    };
  }
}