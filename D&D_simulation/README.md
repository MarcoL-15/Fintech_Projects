# D&D like simulator

This project deviates from the fintech scope of the broader portfolio. The primary objective is to apply information technology and Monte Carlo simulation techniques to analyze tactical decision-making and strategy outcomes in a turn-based combat scenario.

## Project Scope

Rather than focusing on financial instruments or economic phenomena, this work models a **fantasy duel** between two characters (a Magician and a Paladin), each with distinct abilities, cooldowns, and tactical policies. The goal is to quantify win probabilities, optimal strategies, and the impact of different decision thresholds through large-scale stochastic simulation.

## What Was Done

**Game Mechanics:**
- Two-player turn-based combat system with initiative rolls (d20)
- Character attributes: health points, armor class, attack bonuses, damage profiles
- Special abilities: Protection spell (damage mitigation) for the Magician; Heavy attack (high damage) for the Paladin
- Cooldown management for ability reuse

**Decision Policies:**
- **Magician policy:** Cast Protection if off cooldown and health ≤ 60% (or enemy heavy attack is available), else Attack
- **Paladin policy:** Use Heavy attack if cooldown available, else Normal attack

**Simulation Approach:**
Running 10,000+ independent duels with the Monte Carlo method to generate:
- Win rate distributions (Magician vs. Paladin)
- Average duel duration (rounds)
- Ability usage frequencies (Protection casts, Heavy attacks)
- Statistical insights into strategy effectiveness

## Key Insights

The simulation reveals:
- Win probability heavily influenced by character configuration (HP, AC, damage scaling)
- Optimal threshold for Protection casting (HP ratio triggers)
- Cooldown management as a critical strategic lever
- Trade-offs between aggressive and defensive play


