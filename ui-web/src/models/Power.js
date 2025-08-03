export default class Power {
  constructor(data = {}) {
    this.name = data.name || '';
    this.description = data.description || '';
    this.powerPoints = data.powerPoints || 1;
    this.range = data.range || '';
    this.duration = data.duration || '';
    this.trappings = data.trappings || '';
    this.modifiers = data.modifiers || [];
    this.rank = data.rank || 'Novice';
  }

  isValid() {
    return !!(this.name && this.powerPoints > 0);
  }

  addModifier(modifier) {
    this.modifiers.push(modifier);
  }

  removeModifier(index) {
    if (index >= 0 && index < this.modifiers.length) {
      this.modifiers.splice(index, 1);
    }
  }

  getTotalPowerPoints(selectedModifierIndices = []) {
    let total = this.powerPoints;
    selectedModifierIndices.forEach(index => {
      if (this.modifiers[index]) {
        total += this.modifiers[index].powerPointModifier;
      }
    });
    return Math.max(0, total);
  }

  toJSON() {
    return {
      name: this.name,
      description: this.description,
      powerPoints: this.powerPoints,
      range: this.range,
      duration: this.duration,
      trappings: this.trappings,
      modifiers: this.modifiers.map(mod => mod.toJSON ? mod.toJSON() : mod),
      rank: this.rank
    };
  }
}